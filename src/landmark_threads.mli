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

include module type of Landmark
