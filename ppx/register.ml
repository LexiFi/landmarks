(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)

let auto = ref false
let remove = ref false
let threads = Mapper.with_thread

let split c s =
  let open String in
  let res = ref [] in
  let pos = ref 0 in
  let len = length s in
  while
    match index_from s !pos c with
    | exception Not_found ->
        res := sub s !pos (len - !pos) :: !res;
        false
    | k ->
        res := sub s !pos (k - !pos) :: !res;
        pos := k + 1;
        !pos < len || (res := "" :: !res; false)
  do () done;
  List.rev !res

let default_auto, default_remove, default_threads =
  match Sys.getenv "OCAML_LANDMARKS" with
  | exception Not_found -> false, false, false
  | env ->
      let opts = split ',' env in
      List.mem "auto" opts,
      List.mem "remove" opts,
      List.mem "threads" opts

let () =
  let args =
    Arg.["--remove", Set remove, "ignore all landmarks annotations."]
  in
  let mapper _ _ =
    if !remove then Mapper.remove_attributes else Ast_mapper.default_mapper
  in
  let reset_args () =
    remove := default_remove
  in
  Migrate_parsetree.(Driver.register ~reset_args ~args ~name:"landmarks_remove" Versions.ocaml_408 mapper)

let () =
  let args = Arg.[
      "--thread", Set threads, "use the thread-safe version.";
      "--auto", Set auto, "measure all top-level functions."]
  in
  let mapper _ _ =
    if !remove && not !auto then
      Ast_mapper.default_mapper
    else
      Mapper.toplevel_mapper !auto
  in
  let reset_args () =
    auto := default_auto;
    threads := default_threads;
  in
  Migrate_parsetree.(Driver.register
                       ~reset_args ~args ~name:"landmarks" Versions.ocaml_408 mapper)
