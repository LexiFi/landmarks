(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright (C) 2000-2025 LexiFi                                    *)

let default_auto, default_remove, default_threads =
  match Sys.getenv "OCAML_LANDMARKS" with
  | exception Not_found -> false, false, false
  | env ->
      let opts = String.split_on_char ',' env in
      List.mem "auto" opts,
      List.mem "remove" opts,
      List.mem "threads" opts

let auto = ref default_auto
let remove = ref default_remove
let threads = ref default_threads

open Ppxlib

open Ast_helper
open Asttypes
open Parsetree
open Longident
open Location

let mkloc txt loc =
  { txt; loc }

let mknoloc txt =
  mkloc txt !Ast_helper.default_loc

let digest x =
  Digest.to_hex (Digest.string (Marshal.to_string x []))

let error loc code =
  let open Printf in
  let message = function
    | `Too_many_attributes -> "too many attributes"
    | `Expecting_payload l ->
        sprintf "expecting payload in [%s]"
          (String.concat "," (List.map (sprintf "\"%s\"") l))
    | `Payload_not_a_string -> "payload is not a string"
    | `Payload_not_an_expression -> "payload is not an expression"
    | `Provide_a_name -> "this landmark annotation requires a name argument"
  in
  Location.Error.raise (Location.Error.make ~loc ~sub:[]
                          (Printf.sprintf "ppx_landmark: %s" (message code)))

let landmark_hash = ref ""
let landmark_id = ref 0
let landmarks_to_register = ref []

let has_name key {attr_name = {txt; _}; _} = txt = key

let remove_attribute key =
  List.filter (fun x -> not (has_name key x))

let has_attribute ?(auto = false) key l =
  if auto || List.exists (has_name key) l then
    Some (remove_attribute key l)
  else
    None

type landmark =
  | Constant of string
  | Dynamic of Parsetree.expression

let get_payload key = function
    {attr_name = {txt; _}; attr_payload = PStr [{pstr_desc = Pstr_eval ({
        pexp_desc = Pexp_constant (Pconst_string (x, _, None)); _
      }, _); _}]; _} when txt = key ->
      Some (Some (Constant x))
  | {attr_name = {txt; _}; attr_payload = PStr [{pstr_desc = Pstr_eval (expression, _); _}]; _} when txt = key ->
      Some (Some (Dynamic expression))
  | {attr_name = {txt; _}; attr_payload = PStr []; _} when txt = key -> Some None
  | {attr_name = {txt; _}; attr_loc; _} when txt = key -> error attr_loc `Payload_not_an_expression
  | _ -> None

let get_string_payload key ({attr_loc; _} as e) =
  match get_payload key e with
  | Some None -> Some None
  | Some (Some (Constant x)) -> Some (Some x)
  | Some (Some (Dynamic _)) -> error attr_loc `Payload_not_a_string
  | None -> None

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
  let file = l.loc_start.pos_fname in
  let line = l.loc_start.pos_lnum in
  Printf.sprintf "%s:%d" file line

let enter_landmark lm =
  let landmark_enter =
    if !threads then "Landmark_threads.enter" else "Landmark.enter"
  in
  Exp.apply (var landmark_enter) [Nolabel, var lm]
let exit_landmark lm =
  let landmark_exit =
    if !threads then "Landmark_threads.exit" else "Landmark.exit"
  in
  Exp.apply (var landmark_exit) [Nolabel, var lm]

let register_landmark ?id name location =
  let args = [ Labelled "location", Const.string location |> Exp.constant; Nolabel, name] in
  Exp.apply (var "Landmark.register")
    (match id with
     | None -> args
     | Some id -> (Labelled "id", Const.string id |> Exp.constant) :: args)

let register_constant_landmark ?id name location =
  register_landmark ?id (Exp.constant (Const.string name)) location

let new_landmark landmark_name loc =
  incr landmark_id;
  let id = Printf.sprintf "%s_%d" !landmark_hash !landmark_id in
  let landmark = "__generated_landmark_" ^ id in
  let landmark_location = string_of_loc loc in
  landmarks_to_register :=
    (landmark, landmark_name, landmark_location, id) :: !landmarks_to_register;
  landmark

let qualified ctx name = String.concat "." (List.rev (name :: ctx))

let raise_ident = "Landmark.raise"

let unit = Exp.construct (mknoloc (Longident.parse "()")) None

let wrap_landmark ctx landmark loc expr =
  let generate landmark =
    Exp.sequence (enter_landmark landmark)
      (Exp.let_ Nonrecursive
         [Vb.mk (Pat.var (mknoloc "r"))
            (Exp.try_ expr
               [Exp.case (Pat.var (mknoloc "e"))
                  (Exp.sequence
                     (exit_landmark landmark)
                     (Exp.apply (var raise_ident) [Nolabel, var "e"]))])]
         (Exp.sequence
            (exit_landmark landmark)
            (var "r")))
  in
  match landmark with
  | Constant landmark_name ->
      let landmark_name = qualified ctx landmark_name in
      let landmark = new_landmark landmark_name loc in
      generate landmark
  | Dynamic expression ->
      let landmark = Printf.sprintf "__dynamic_landmark__%s" !landmark_hash in
      (Exp.ifthenelse
         (Exp.apply (var "Landmark.profiling") [Nolabel, unit])
         (Exp.let_ Nonrecursive
            [Vb.mk
               (Pat.var
                  (mknoloc landmark)
               )
               (register_landmark expression (string_of_loc loc))
            ]
            (generate landmark)
         )
         (Some expr)
      )


let rec arity {pexp_desc; _} =
  match pexp_desc with
  | Pexp_fun (a, _, _, e) -> a :: arity e
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
  | Pexp_newtype (_, e) -> arity e
  | Pexp_constraint (e, _) -> arity e
  | Pexp_poly (e, _) -> arity e
  | _ -> []

let rec wrap_landmark_method ctx landmark loc ({pexp_desc; _} as expr) =
  match pexp_desc with
  | Pexp_fun (label, def, pat, e) ->
      { expr with pexp_desc = Pexp_fun (label, def, pat, wrap_landmark_method ctx landmark loc e)}
  | Pexp_poly (e, typ) ->
      { expr with pexp_desc = Pexp_poly (wrap_landmark_method ctx landmark loc e, typ)}
  | _ -> wrap_landmark ctx landmark loc expr

let eta_expand f t n =
  let vars =
    List.mapi (fun k x -> (x, Printf.sprintf "__x%d" k)) n
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

let rec not_a_constant expr = match expr.pexp_desc with
  | Pexp_constant _ | Pexp_ident _ -> false
  | Pexp_coerce (e, _, _) | Pexp_poly (e, _) | Pexp_constraint (e, _) -> not_a_constant e
  | _ -> true

let rec name_of_pattern pat =
  match pat.ppat_desc with
  | Ppat_var {txt; _} -> Some txt
  | Ppat_constraint (pat, _) -> name_of_pattern pat
  | _ -> None

let translate_value_bindings ctx value_binding auto vbs =
  let vbs_arity_name =
    List.map
      (fun vb -> match vb, has_landmark_attribute ~auto vb.pvb_attributes with
         | { pvb_expr; pvb_loc; pvb_pat; _}, Some attr
           when not_a_constant pvb_expr ->
             let arity = arity pvb_expr in
             let from_names arity fun_name landmark_name =
               if auto && arity = [] then
                 (vb, None)
               else
                 (vb, Some (arity, fun_name, landmark_name, pvb_loc, attr))
             in
             (match name_of_pattern pvb_pat,
                    filter_map (get_payload "landmark") vb.pvb_attributes
              with
              | Some fun_name, []
              | Some fun_name, [ None ] ->
                  from_names arity fun_name (Constant fun_name)
              | Some fun_name, [ Some landmark ] ->
                  from_names arity fun_name landmark
              | _, [Some name] -> from_names [] "" name
              | _, [None] -> error pvb_loc `Provide_a_name
              | _, [] -> (vb, None)
              | _, _ :: _ :: _ -> error pvb_loc `Too_many_attributes
             )
         | _, _ -> (vb, None))
      vbs
  in
  let vbs = List.map (function
      | (vb, None) -> value_binding vb
      | {pvb_pat; pvb_loc; pvb_expr; _}, Some (arity, _, name, loc, attrs) ->
          (* Remove landmark attribute: *)
          let vb =
            Vb.mk ~attrs ~loc:pvb_loc pvb_pat pvb_expr
            |> value_binding
          in
          if arity = [] then
            { vb with pvb_expr = wrap_landmark ctx name loc vb.pvb_expr}
          else
            vb) vbs_arity_name
  in
  let new_vbs = filter_map (function
      | (_, Some (_ :: _ as arity, fun_name, landmark_name, loc, _)) ->
          let ident = Exp.ident (mknoloc (Lident fun_name)) in
          let expr = eta_expand (wrap_landmark ctx landmark_name loc) ident arity in
          Some (Vb.mk (Pat.var (mknoloc fun_name)) expr)
      | _ -> None) vbs_arity_name
  in
  vbs, new_vbs

let mapper =
  object(self)
    inherit [bool * string list] Ast_traverse.fold_map as super

    method! module_binding ({pmb_name; _} as binding) ((auto, ctx) as acc) =
      let acc =
        match pmb_name.txt with
        | None -> acc
        | Some txt -> auto, txt :: ctx
      in
      let result, (_, ctx) = super#module_binding binding acc in
      result, (auto, ctx)

    method! structure l (auto, ctx) =
      let _, results =
        List.fold_left (fun (auto, acc) expr ->
            match expr with
            | { pstr_desc = Pstr_attribute attr; pstr_loc; _} as pstr ->
                (match get_string_payload "landmark" attr with
                 | Some (Some "auto") -> true, acc
                 | Some (Some "auto-off") -> false, acc
                 | None -> auto, pstr :: acc
                 | _ -> error pstr_loc (`Expecting_payload ["auto"; "auto-off"]))
            | { pstr_desc = Pstr_value (rec_flag, vbs); pstr_loc} ->
                let value_binding vb =
                  fst (self # value_binding vb (auto, ctx))
                in
                let vbs, new_vbs =
                  translate_value_bindings ctx value_binding auto vbs
                in
                let str = Str.value ~loc:pstr_loc rec_flag vbs in
                if new_vbs = [] then auto, str :: acc
                else
                  let warning_off =
                    Str.attribute {attr_name = mknoloc "ocaml.warning"; attr_payload = payload_of_string "-32";
                                   attr_loc = Location.none}
                  in
                  let include_wrapper = new_vbs
                                        |> Str.value Nonrecursive
                                        |> fun x -> Mod.structure [warning_off; x]
                                                    |> Incl.mk
                                                    |> Str.include_
                  in
                  auto, include_wrapper :: str :: acc
            | sti ->
                let sti, _ = super # structure_item sti (auto, ctx) in
                auto, sti :: acc) (auto, []) l
      in
      List.rev results, (auto, ctx)

    method! class_field class_field ((auto, ctx) as acc) =
      match class_field with
      | { pcf_desc = Pcf_method (loc, privat, Cfk_concrete (flag, expr)); pcf_loc; pcf_attributes; _ } ->
          begin
            let landmark =
              match filter_map (get_payload "landmark") pcf_attributes, auto with
              | [Some landmark_name], _ -> Some landmark_name
              | [None], _ | _, true -> Some (Constant loc.txt)
              | [], false -> None
              | _ :: _ :: _, _ -> error pcf_loc `Too_many_attributes
            in
            match landmark with
            | None ->
                super # class_field class_field acc
            | Some landmark ->
                let expr =
                  wrap_landmark_method ctx landmark pcf_loc (fst (self # expression expr acc))
                in
                { class_field with
                  pcf_desc = Pcf_method (loc, privat, Cfk_concrete (flag, expr));
                  pcf_attributes = remove_attribute "landmark" pcf_attributes
                }, acc
          end
      | _ -> super # class_field class_field acc

    method! class_expr class_expr ((_, ctx) as acc) =
      match class_expr with
      | {pcl_desc = Pcl_let (rec_flag, vbs, body); _} ->
          let vbs, new_vbs =
            let value_binding vb =
              fst (self # value_binding vb acc)
            in
            translate_value_bindings ctx value_binding false vbs
          in
          let body, _ = self # class_expr body acc in
          let body =
            if new_vbs = [] then
              body
            else
              Cl.let_ Nonrecursive new_vbs body
          in
          { class_expr with pcl_desc = Pcl_let (rec_flag, vbs, body) }, acc
      | _ -> super # class_expr class_expr acc

    method! expression expr ((_, ctx) as acc) =
      let expr = match expr with
        | ({pexp_desc = Pexp_let (rec_flag, vbs, body); _} as expr) ->
            let vbs, new_vbs =
              let value_binding vb =
                fst (self # value_binding vb acc)
              in
              translate_value_bindings ctx value_binding false vbs
            in
            let body = fst (self # expression body acc) in
            let body =
              if new_vbs = [] then
                body
              else
                Exp.let_ Nonrecursive new_vbs body
            in
            { expr with pexp_desc = Pexp_let (rec_flag, vbs, body) }
        | expr -> fst (super # expression expr acc)
      in
      let {pexp_attributes; pexp_loc; _} = expr in
      match filter_map (get_payload "landmark") pexp_attributes with
      | [Some landmark_name] ->
          { expr with pexp_attributes =
                        remove_attribute "landmark" pexp_attributes }
          |> wrap_landmark ctx landmark_name pexp_loc, acc
      | [ None ] -> error pexp_loc `Provide_a_name
      | [] -> expr, acc
      | _ -> error pexp_loc `Too_many_attributes

  end

let remove_attributes =
  object
    inherit Ast_traverse.map as super

    method! structure l =
      let l =
        List.filter (function {pstr_desc = Pstr_attribute attr; _ }
            when has_landmark_attribute [attr] <> None -> false | _ -> true) l
      in
      super # structure l

    method! attributes attributes =
      super # attributes
        (match has_landmark_attribute attributes with
         | Some attrs ->
             attrs
         | None ->
             attributes)
  end

let has_disable l =
  let disable = ref false in
  let f = function
    | { pstr_desc = Pstr_attribute attr; pstr_loc; _} as pstr ->
        (match get_string_payload "landmark" attr with
         | Some (Some "disable") -> disable := true; None
         | Some (Some "auto-off") | Some (Some "auto") | None -> Some pstr
         | _ -> error pstr_loc
                  (`Expecting_payload ["auto"; "auto-off"; "disable"]))
    | i -> Some i
  in
  let res = filter_map f l in
  !disable, res

let toplevel_mapper auto =
  object
    inherit Ast_traverse.map

    method! signature si = si

    method! structure l =
      match l with [] -> [] | l ->
        assert (!landmark_hash = "");
        landmark_hash := digest l;
        let disable, l = has_disable l in
        if disable then l else begin
          let first_loc = (List.hd l).pstr_loc in
          let module_name = Filename.remove_extension (Filename.basename !Ocaml_common.Location.input_name) in
          let ctx = [String.capitalize_ascii module_name] in
          let l, _ = mapper # structure l (auto, ctx) in
          let landmark_name = Printf.sprintf "load(%s)" module_name in
          let lm =
            if auto then
              Some (new_landmark landmark_name first_loc)
            else
              None
          in
          if !landmarks_to_register = [] then l else
            let landmarks =
              Str.value Nonrecursive
                (List.map (fun (landmark, landmark_name, landmark_location, id) ->
                     Vb.mk (Pat.var (mknoloc landmark))
                       (register_constant_landmark ~id landmark_name landmark_location))
                    (List.rev !landmarks_to_register))
            in
            match lm with
            | Some lm ->
                let begin_load =
                  Str.value Nonrecursive
                    [Vb.mk (Pat.construct (mknoloc (Longident.parse "()")) None)
                       (enter_landmark lm)]
                in
                let exit_load =
                  Str.value Nonrecursive
                    [Vb.mk (Pat.construct (mknoloc (Longident.parse "()")) None)
                       (exit_landmark lm)]
                in
                landmarks :: (begin_load :: l @ [exit_load])
            | None ->
                landmarks :: l
        end
  end
