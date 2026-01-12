(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright (C) 2000-2025 LexiFi                                    *)

open Utils

type t

val get_state: unit -> t

val landmark_root : t -> landmark
val dummy_node : t -> node
val dummy_key : t -> landmark_key

val profiling : t -> bool
val set_profiling : t -> bool -> unit

val get_landmarks_of_key : t -> W.t
val add_landmarks_of_key : t -> landmark_key -> unit
val get_ds_landmark: t -> landmark -> landmark

val get_node_id_ref : t -> int
val set_node_id_ref : t -> int -> unit
val get_allocated_nodes : t -> node list
val set_allocated_nodes : t -> node list -> unit

val new_node : t -> landmark -> node

val get_current_root_node : t -> node
val set_current_root_node : t -> node -> unit
val get_current_node_ref : t -> node
val set_current_node_ref : t -> node -> unit
val get_cache_miss_ref : t -> int
val set_cache_miss_ref : t -> int -> unit

val incr_cache_miss_ref : t -> unit

val get_profiling_stack :
  t ->
  (profiling_state, profiling_state array) Utils.Stack.t

val clear_cache : t -> unit
val reset: t -> unit
val export : merge:(node -> Graph.graph -> unit) -> ?label:string -> t -> Graph.graph
