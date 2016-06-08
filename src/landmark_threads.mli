(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)

(** Protects landmarks against concurrency. *)


(** This module implements of wrapper around some of the primitives
    of the Landmark module in order to prevent concurrent thread to
    enter landmarks. Only one thread may be benchmarked at a time,
    the one which started the profiling. Other calls to the
    primitives outside this thread behaves as ignore.*)

(** It requires the Thread module (and only available in
    'landmarks-threads' archive. *)

open Landmark

val enter: landmark -> unit
val exit: landmark -> unit
val increment: ?times:int -> counter -> unit
val sample: sampler -> float -> unit
val wrap: landmark -> ('a -> 'b) -> 'a -> 'b
val unsafe_wrap: landmark -> ('a -> 'b) -> 'a -> 'b
val reset: unit -> unit
val export: unit -> Landmark_graph.graph
val export_and_reset: unit -> Landmark_graph.graph
val start_profiling: ?profiling_options:profiling_options -> unit -> unit
val stop_profiling: unit -> unit
