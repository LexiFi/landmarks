(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)

open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident
open Location

let landmark_id = ref 0
let landmarks_to_register = ref []

let has_name key ({txt; _}, _) = txt = key

let rec has_attribute ?(auto = false) key l =
  if auto || List.exists (has_name key) l then
    Some (List.filter (fun x -> not (has_name key x)) l)
  else
    None

let has_landmark_attribute ?auto = has_attribute ?auto "landmark"

let payload_of_string x =
  PStr [Str.eval (Exp.constant (Const.string x))]

let var x = Exp.ident (mknoloc (Longident.parse x))

let rec filter_map f = function
  | [] -> []
  | hd :: tl ->
     match f hd with
     | Some x -> x :: (filter_map f tl)
     | None -> filter_map f tl

let rec string_of_pattern pat =
 match pat.ppat_desc with
 | Ppat_any -> "_"
 | Ppat_var {txt; _} -> txt
 | Ppat_lazy p
 | Ppat_constraint (p, _)
 | Ppat_exception p
 | Ppat_construct (_, Some p)
 | Ppat_alias (p, _) -> string_of_pattern p
 | Ppat_construct (c, None) -> String.concat "." (Longident.flatten c.txt)
 | Ppat_constant _ -> "CONSTANT"
 | Ppat_interval _ -> "INTERVAL"
 | Ppat_array l
 | Ppat_tuple l -> String.concat "_" (List.map string_of_pattern l)
 | Ppat_variant (x, None) -> x
 | Ppat_variant (x, Some p) -> x ^ "_" ^ string_of_pattern p
 | Ppat_record (l, _) -> String.concat "_" (List.map (fun (_, x) -> string_of_pattern x) l)
 | Ppat_or (a, b) -> string_of_pattern a ^ "_" ^ string_of_pattern b
 | Ppat_type _ -> "TYPE"
 | Ppat_extension _ -> "EXTENSION"
 | Ppat_unpack m -> m.txt

let string_of_loc (l : Location.t) = Format.asprintf "%a" Location.print_loc l

let begin_landmark lm = Exp.apply (var "Landmark.enter") [Nolabel, var lm]
let end_landmark lm = Exp.apply (var "Landmark.exit") [Nolabel, var lm]
let register_landmark name filename =
  Exp.apply (var "Landmark.register")
    [Nolabel, Const.string name |> Exp.constant;
     Labelled "filename", Const.string filename |> Exp.constant]


let rec wrap_landmark landmark expr =
  match expr.pexp_desc with
  | Pexp_fun (l,o,p,e) -> Exp.fun_ l o p (wrap_landmark landmark e)
  | _ ->
      Exp.sequence (begin_landmark landmark)
       (Exp.let_ Nonrecursive
          [Vb.mk (Pat.var (mknoloc "r"))
             (Exp.try_ expr
                [Exp.case (Pat.var (mknoloc "e"))
                   (Exp.sequence
                      (end_landmark landmark)
                      (Exp.apply (var "raise") [Nolabel, var "e"]))])]
             (Exp.sequence
                (end_landmark landmark)
                (var "r")))

let new_landmark landmark_name loc =
  incr landmark_id;
  let landmark = Printf.sprintf "__generated_landmark_%d" !landmark_id in
  let landmark_filename = string_of_loc loc in
  landmarks_to_register :=
    (landmark, landmark_name, landmark_filename) :: !landmarks_to_register;
  landmark

let min_list l1 l2 =
  if List.length l1 < List.length l2 then
    l1
  else
    l2

let rec arity {pexp_desc; _} =
  match pexp_desc with
  | Pexp_fun (a, _, _, e ) -> a :: arity e
  | Pexp_function cases ->
    Nolabel :: (List.fold_left
      (fun acc {pc_rhs; _} -> min_list (arity pc_rhs) acc)
      [] cases)
  | _ -> []

let eta_expand f t n =
  let k = ref 0 in
  let vars =
    List.map (fun x -> match x with
        | Nolabel -> incr k; (x, Printf.sprintf "x%d" !k)
        | Optional s | Labelled s -> (x, s)) n
  in
  let rec app acc = function
    | [] -> acc
    | (l,x) :: tl -> app (Exp.apply acc [l, Exp.ident (mknoloc (Lident x))]) tl
  in
  let rec lam = function
    | [] -> f (app t vars)
    | (l,x) :: tl -> Exp.fun_ l None (Pat.var (mknoloc x)) (lam tl)
  in
  lam vars


let rec deep_mapper auto =
  { default_mapper with
    structure = (fun mapper l ->
      List.map
        (function
          | { pstr_desc = Pstr_value (Recursive, vbs); pstr_loc} ->
            let vbs_arity_name =
              List.map
                (fun vb -> match vb, has_landmark_attribute ~auto vb.pvb_attributes with
                  | { pvb_expr; pvb_loc;
                       pvb_pat =
                         {ppat_desc =
                            Ppat_var {txt = name; _}; _};
                       _}, Some attr ->
                    (vb, Some (arity pvb_expr, name, pvb_loc, attr))
                  | _, _ -> (vb, None))
                vbs
            in
            let vbs = List.map (function
                | (vb, None) ->
                  mapper.value_binding (deep_mapper false) vb
                | {pvb_pat; pvb_loc; pvb_expr; _}, Some (_, _, _, attrs) ->
                  (* Remove landmark attribute: *)
                  let vb = Vb.mk ~attrs ~loc:pvb_loc pvb_pat pvb_expr in
                  default_mapper.value_binding (deep_mapper false) vb) vbs_arity_name
            in
            let str = Str.value ~loc:pstr_loc Recursive vbs in
            let arity_name = filter_map snd vbs_arity_name in
            if arity_name = [] then [str]
            else
              let warning_off =
                Str.attribute (mknoloc "ocaml.warning", payload_of_string "-32")
              in
              let include_wrapper =
                List.map (fun (arity, name, loc, _) ->
                    let ident = Exp.ident (mknoloc (Lident name)) in
                    let landmark = new_landmark name loc in
                    let expr = eta_expand (wrap_landmark landmark) ident arity in
                    Vb.mk (Pat.var (mknoloc name)) expr
                  ) arity_name
                |> Str.value Nonrecursive
                |> fun x -> Mod.structure [warning_off; x]
                            |> Incl.mk
                            |> Str.include_
              in
              [str; include_wrapper]
          | sti -> [mapper.structure_item mapper sti])
        l |> List.flatten);
    value_binding =
      fun mapper ({pvb_pat; pvb_expr; pvb_attributes; _} as vb) ->
        let pvb_expr = mapper.expr (deep_mapper false) pvb_expr in
        match has_landmark_attribute ~auto pvb_attributes with
        | Some pvb_attributes ->
          let landmark_name = string_of_pattern pvb_pat in
          let landmark = new_landmark landmark_name pvb_pat.ppat_loc in
          {vb with pvb_expr = wrap_landmark landmark pvb_expr; pvb_attributes}
        | None ->
          {vb with pvb_expr}
  }

let shallow_mapper auto =
  let mapper = deep_mapper auto in
  { mapper with
     structure = fun _ l ->
       let l = (deep_mapper auto).structure (deep_mapper auto) l in
       if !landmarks_to_register = [] then l else
       let landmarks =
         Str.value Nonrecursive
           (List.map (fun (landmark, landmark_name, landmark_filename) ->
             Vb.mk (Pat.var (mknoloc landmark))
                   (register_landmark landmark_name landmark_filename)) (List.rev !landmarks_to_register))
      in landmarks :: l }

let remove_attributes =
  { default_mapper with
     attributes = fun mapper attributes ->
       match has_landmark_attribute attributes with
       | Some attrs -> attrs
       | None -> attributes }

let () = register "landmarks" (fun _ ->
    match Sys.getenv "OCAML_LANDMARKS" with
    | exception Not_found -> shallow_mapper false
    | "0" -> remove_attributes
    | "auto" -> shallow_mapper true
    | _ -> shallow_mapper false)
