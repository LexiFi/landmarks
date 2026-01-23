(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright (C) 2000-2025 LexiFi                                    *)

open Utils

type t

type landmark

val landmark_of_landmark_body: t -> landmark_body -> landmark
val get_ds_landmark: t -> landmark -> landmark_body

val init:
  reset_state:(t -> unit) ->
  new_node:(t -> landmark_body -> node) ->
  stop_profiling:(t -> unit) ->
  export:(t -> string -> Graph.graph) ->
  unit ->
  t

val dummy_landmark : t -> landmark
val landmark_root : t -> landmark_body
val dummy_node : t -> node

val profiling : t -> bool
val set_profiling : t -> bool -> unit

val get_node_id_ref : t -> int
val set_node_id_ref : t -> int -> unit
val get_incr_node_id_ref : t -> int
val get_allocated_nodes : t -> node list
val set_allocated_nodes : t -> node list -> unit

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

val clear_cache : ((landmark -> unit) -> unit) -> t -> unit
val export :
  export:(t -> string -> Graph.graph) ->
  merge:(t -> node -> Graph.graph -> unit) ->
  ?label:string -> t -> Graph.graph
