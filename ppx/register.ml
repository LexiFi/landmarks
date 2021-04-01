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

open Ppxlib

let () =
  Driver.add_arg "--thread" (Set threads) ~doc:"use the thread-safe version.";
  Driver.add_arg "--auto" (Set auto) ~doc:"measure all top-level functions.";
  let impl str =
    if !remove && not !auto then
      str
    else
      (Mapper.toplevel_mapper !auto) # structure str
  in
  Ppxlib.Driver.register_transformation "landmarks" ~impl


let () =
  Driver.add_arg "--remove" (Set remove) ~doc:"ignore all landmarks annotations.";
  let impl str  =
    if !remove then
      Mapper.remove_attributes # structure str
    else
      str
  in
  let intf sg  =
    if !remove then
      Mapper.remove_attributes # signature sg
    else
      sg
  in
  Ppxlib.Driver.register_transformation
    "landmarks_remove"
    ~impl
    ~intf
