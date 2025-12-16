(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright (C) 2000-2025 LexiFi                                    *)

open Utils

module type S =
sig
  val landmark_root : unit -> landmark
  val dummy_node : unit -> node
  val dummy_key : unit -> landmark_key

  val profiling : unit -> bool
  val set_profiling : bool -> unit

  val get_landmarks_of_key : unit -> W.t
  val add_landmarks_of_key : landmark_key -> unit
  val get_landmark_body: landmark -> landmark

  val get_node_id_ref : unit -> int
  val set_node_id_ref : int -> unit
  val get_allocated_nodes : unit -> node list
  val set_allocated_nodes : node list -> unit

  val new_node : landmark -> node

  val get_current_root_node : unit -> node
  val set_current_root_node : node -> unit
  val get_current_node_ref : unit -> node
  val set_current_node_ref : node -> unit
  val get_cache_miss_ref : unit -> int
  val set_cache_miss_ref : int -> unit

  val incr_cache_miss_ref : unit -> unit

  val get_profiling_stack :
    unit ->
    (profiling_state, profiling_state array) Utils.Stack.t

  val clear_cache : unit -> unit
  val reset: unit -> unit
  val export : merge:(node -> Graph.graph -> unit) -> ?label:string -> unit -> Graph.graph
end
