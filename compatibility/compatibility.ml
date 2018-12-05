(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)

open Ast_mapper
open Parsetree
open Longident
open Location

let substitute_capitalize_ascii =
  { default_mapper with
    expr =
      begin fun mapper -> function
        | ({pexp_desc =
              Pexp_ident ({txt = Ldot (Lident "String", "capitalize_ascii"); _} as ident); _} as expr) ->
          { expr with pexp_desc = Pexp_ident { ident with txt = Ldot (Lident "String", "capitalize")}}
        | expr -> default_mapper.expr mapper expr
      end
  }

let string_start s1 s2 =
  String.length s1 >= String.length s2 &&
  String.sub s1 0 (String.length s2) = s2

let mapper _ _ =
  if string_start Sys.ocaml_version "4.02" then
    substitute_capitalize_ascii
  else
    default_mapper

let () =
  Migrate_parsetree.(Driver.register ~name:"compatibility" Versions.ocaml_404 mapper)
