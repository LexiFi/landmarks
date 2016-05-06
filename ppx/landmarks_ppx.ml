open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident
open Location

let landmark_id = ref 0 
let landmarks_to_register = ref []

let has_name key ({txt; _}, _) = txt = key

let var x = Exp.ident (mknoloc (Longident.parse x))

let rec string_of_pattern pat = 
 match pat.ppat_desc with
 | Ppat_any -> "_"
 | Ppat_var {txt; _} -> txt
 | Ppat_alias (pat, _) -> string_of_pattern pat
 | Ppat_constant _ -> "constant"
 | Ppat_interval _ -> "interval"
 | Ppat_tuple l -> String.concat "_" (List.map string_of_pattern l)
 | Ppat_variant (x, None) -> x
 | Ppat_variant (x, Some p) -> x ^ "_" ^ string_of_pattern p
 | _ -> "name_generator_not_implemented"

let string_of_loc (l : Location.t) = Format.asprintf "%a" Location.print_loc l

let begin_landmark lm = Exp.apply (var "Landmark.enter") [Nolabel, var lm]
let end_landmark lm = Exp.apply (var "Landmark.exit") [Nolabel, var lm]
let register_landmark name filename = Exp.apply (var "Landmark.register") [Nolabel, Const.string name |> Exp.constant; Labelled "filename", Const.string filename |> Exp.constant]


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

let deep_mapper =
  { default_mapper with
    value_binding =
      fun mapper ({pvb_pat; pvb_expr; pvb_attributes; _} as vb) ->
        let pvb_expr = mapper.expr mapper pvb_expr in
        if List.exists (has_name "landmark") pvb_attributes then begin
          let pvb_attributes = List.filter (fun x -> not (has_name "landmark" x)) pvb_attributes in
          incr landmark_id;
          let landmark = Printf.sprintf "__generated_landmark_%d" !landmark_id in
          let landmark_name = string_of_pattern pvb_pat in
          let landmark_filename = string_of_loc pvb_pat.ppat_loc in
          landmarks_to_register := 
            (landmark, landmark_name, landmark_filename) :: !landmarks_to_register;
          {vb with pvb_expr = wrap_landmark landmark pvb_expr; pvb_attributes}
        end else 
          {vb with pvb_expr}
  }

let shallow_mapper = 
  { deep_mapper with 
     structure = fun _ l -> 
       let l = List.map (deep_mapper.structure_item deep_mapper) l in 
       let landmarks = 
         Str.value Nonrecursive
           (List.map (fun (landmark, landmark_name, landmark_filename) ->
             Vb.mk (Pat.var (mknoloc landmark))
                   (register_landmark landmark_name landmark_filename)) (List.rev !landmarks_to_register))
      in landmarks :: l }
            
 
       

let () = register "landmarks" (fun _ -> shallow_mapper)
