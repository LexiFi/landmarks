(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright (C) 2000-2025 LexiFi                                    *)

let auto = Mapper.auto
let remove = Mapper.remove
let threads = Mapper.threads

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
