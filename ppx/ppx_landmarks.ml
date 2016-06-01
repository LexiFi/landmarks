(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)


open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident
open Location

let warning loc code =
  let open Format in
  let message = function
  | `Let_expect_function -> "let annotation expect explicit eta-long function (the body should start with a 'fun ...' or 'function ...')"
  in
  printf "%a, ppx_landmark: %s\n%!" print_loc loc (message code)

let error loc code =
  let open Printf in
  let message = function
  | `Too_many_attributes -> "too many attributes"
  | `Expecting_payload l ->
    sprintf "expecting payload in [%s]"
      (String.concat "," (List.map (sprintf "\"%s\"") l))
  | `Payload_not_a_string -> "payload is not a string"
  | `Provide_a_name -> "this landmark annotation requires a name argument"
  in
  raise (Location.Error (Location.error ~loc
			  (Printf.sprintf "ppx_landmark: %s" (message code))))

let landmark_id = ref 0
let landmarks_to_register = ref []

let has_name key ({txt; _}, _) = txt = key

let remove_attribute key =
  List.filter (fun x -> not (has_name key x))

let has_attribute ?(auto = false) key l =
  if auto || List.exists (has_name key) l then
    Some (remove_attribute key l)
  else
    None

let get_string_payload key = function
    {txt; _}, PStr [{pstr_desc = Pstr_eval ({
        pexp_desc = Pexp_constant (Pconst_string (x, None)); _
      }, _); _}] when txt = key -> Some (Some x)
  | {txt; loc}, PStr [] when txt = key -> Some None
  | {txt; loc}, _ when txt = key -> error loc `Payload_not_a_string
  | _ -> None

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

let string_of_loc (l : Location.t) =
  let file, line, _ = Location.get_pos_info l.loc_start in
  Printf.sprintf "%s:%d" (Location.show_filename file) line

let enter_landmark lm = Exp.apply (var "Landmark.enter") [Nolabel, var lm]
let exit_landmark lm = Exp.apply (var "Landmark.exit") [Nolabel, var lm]
let register_landmark name location =
  Exp.apply (var "Landmark.register")
    [ Labelled "location", Const.string location |> Exp.constant;
      Nolabel, Const.string name |> Exp.constant]

let new_landmark landmark_name loc =
  incr landmark_id;
  let landmark = Printf.sprintf "__generated_landmark_%d" !landmark_id in
  let landmark_location = string_of_loc loc in
  landmarks_to_register :=
    (landmark, landmark_name, landmark_location) :: !landmarks_to_register;
  landmark

let rec wrap_landmark landmark_name loc expr =
  let landmark = new_landmark landmark_name loc in
  Exp.sequence (enter_landmark landmark)
  (Exp.let_ Nonrecursive
    [Vb.mk (Pat.var (mknoloc "r"))
       (Exp.try_ expr
          [Exp.case (Pat.var (mknoloc "e"))
             (Exp.sequence
                (exit_landmark landmark)
                   (Exp.apply (var "Pervasives.raise") [Nolabel, var "e"]))])]
                   (Exp.sequence
       (exit_landmark landmark)
          (var "r")))

let rec arity {pexp_desc; _} =
  match pexp_desc with
  | Pexp_fun (a, _, _, e ) -> a :: arity e
  | Pexp_function cases ->
    let max_list l1 l2 =
      if List.length l1 < List.length l2 then
        l1
      else
        l2
    in
    Nolabel :: (List.fold_left
      (fun acc {pc_rhs; _} -> max_list (arity pc_rhs) acc)
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

let rec translate_value_bindings deep_mapper auto rec_flag vbs =
  let vbs_arity_name =
    List.map
      (fun vb -> match vb, has_landmark_attribute ~auto vb.pvb_attributes with
         | { pvb_expr; pvb_loc;
             pvb_pat =
               {ppat_desc =
                  Ppat_var {txt = name; _}; _};
             _}, Some attr ->
           let name =
             match filter_map (get_string_payload "landmark") vb.pvb_attributes with
             | [Some name] -> name
             | [] | [ None ] -> name
             | _ -> error pvb_loc `Too_many_attributes
           in
           let arity = arity pvb_expr in
           if arity = [] then (vb, None)
           else (vb, Some (arity, name, pvb_loc, attr))
         | { pvb_loc; _}, Some _ when not auto -> warning pvb_loc `Let_expect_function; (vb, None)
         | _, _ -> (vb, None))
      vbs
  in
  let vbs = List.map (function
      | (vb, None) ->
        default_mapper.value_binding (deep_mapper false) vb
      | {pvb_pat; pvb_loc; pvb_expr; _}, Some (_, _, _, attrs) ->
        (* Remove landmark attribute: *)
        let vb = Vb.mk ~attrs ~loc:pvb_loc pvb_pat pvb_expr in
        default_mapper.value_binding (deep_mapper false) vb) vbs_arity_name
  in
  let new_vbs = filter_map (function
      | (_, Some (arity, name, loc, _)) ->
        let ident = Exp.ident (mknoloc (Lident name)) in
        let expr = eta_expand (wrap_landmark name loc) ident arity in
        Some (Vb.mk (Pat.var (mknoloc name)) expr)
      | _ -> None) vbs_arity_name
  in
  vbs, new_vbs

let auto = ref false

let rec deep_mapper shallow =
  { default_mapper with
    structure = (fun mapper l ->
      List.map
        (function
          | { pstr_desc = Pstr_attribute attr; pstr_loc; _} as pstr ->
            (match get_string_payload "landmark" attr with
            | Some (Some "auto") -> auto := true; []
            | Some (Some "auto-off") -> auto := false; []
            | None -> [pstr]
            | _ -> error pstr_loc (`Expecting_payload ["auto"; "auto-off"]))
          | { pstr_desc = Pstr_value (rec_flag, vbs); pstr_loc} ->
            let vbs, new_vbs =
              translate_value_bindings deep_mapper (shallow && !auto) rec_flag vbs
            in
            let str = Str.value ~loc:pstr_loc rec_flag vbs in
            if new_vbs = [] then [str]
            else
              let warning_off =
                Str.attribute (mknoloc "ocaml.warning", payload_of_string "-32")
              in
              let include_wrapper = new_vbs
                |> Str.value Nonrecursive
                |> fun x -> Mod.structure [warning_off; x]
                            |> Incl.mk
                            |> Str.include_
              in
              [str; include_wrapper]
          | sti -> [mapper.structure_item mapper sti])
        l |> List.flatten);

    expr =
      fun mapper -> function
        | ({pexp_desc = Pexp_let (rec_flag, vbs, body); _} as expr) ->
          let vbs, new_vbs =
            translate_value_bindings deep_mapper (shallow && !auto) rec_flag vbs
          in
          let body =
            if new_vbs = [] then
              body
            else
              Exp.let_ Nonrecursive new_vbs body
          in
          { expr with pexp_desc = Pexp_let (rec_flag, vbs, body) }

        | ({pexp_attributes; pexp_loc; _} as expr) ->
          let expr = default_mapper.expr (deep_mapper shallow) expr in
          match filter_map (get_string_payload "landmark") pexp_attributes with
          | [Some landmark_name] ->
               {expr with pexp_attributes = remove_attribute "landmark" pexp_attributes}
            |> wrap_landmark landmark_name pexp_loc
          | [ None ] -> error pexp_loc `Provide_a_name
          | [] -> expr
          | _ -> error pexp_loc `Too_many_attributes
  }

let shallow_mapper =
  let mapper = deep_mapper true in
  { mapper with
     structure = fun _ -> function [] -> []
       | l ->
       let first_loc = (List.hd l).pstr_loc in
       let l = mapper.structure mapper l in
       let landmark_name = Printf.sprintf "load(%s)" !Location.input_name in
       let lm = if !auto then Some (new_landmark landmark_name first_loc) else None in
       if !landmarks_to_register = [] then l else
       let landmarks =
         Str.value Nonrecursive
           (List.map (fun (landmark, landmark_name, landmark_location) ->
             Vb.mk (Pat.var (mknoloc landmark))
                   (register_landmark landmark_name landmark_location))
               (List.rev !landmarks_to_register))
       in
       match lm with
       | Some lm ->
         let begin_load =
           Str.value Nonrecursive
            [Vb.mk (Pat.construct (mknoloc (Longident.parse "()")) None) (enter_landmark lm)]
         in
         let exit_load =
           Str.value Nonrecursive
             [Vb.mk (Pat.construct (mknoloc (Longident.parse "()")) None) (exit_landmark lm)]
         in
         landmarks :: (begin_load :: l @ [exit_load])
       | None ->
         landmarks :: l }

let remove_attributes =
  { default_mapper with
     attributes = fun mapper attributes ->
       match has_landmark_attribute attributes with
       | Some attrs -> attrs
       | None -> attributes }

let () = register "landmarks" (fun _ ->
    match Sys.getenv "OCAML_LANDMARKS" with
    | exception Not_found -> shallow_mapper
    | env ->
      let opts = Landmark_misc.split ',' env in
      if List.mem "remove" opts then
        remove_attributes
      else begin
        auto := List.mem "auto" opts;
        shallow_mapper
      end)
