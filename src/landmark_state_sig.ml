(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright (C) 2000-2025 LexiFi                                    *)

module type S =
sig
  val profiling : unit -> bool
  val set_profiling : bool -> unit
  val get_last_landmark_id : unit -> int
  val incr_last_landmark_id : unit -> unit
  val get_landmarks_of_key : unit -> Utils.W.t
  val add_landmarks_of_key : Utils.landmark_key -> unit
  val get_node_id_ref : unit -> int
  val set_node_id_ref : int -> unit
  val get_allocated_nodes : unit -> Utils.node list
  val set_allocated_nodes : Utils.node list -> unit
  val set_profiling_options : Utils.profiling_options -> unit
  val profiling_options : unit -> Utils.profiling_options
  val profile_with_debug : unit -> bool
  val profile_with_allocated_bytes : unit -> bool
  val profile_with_sys_time : unit -> bool
  val profile_output : unit -> Utils.profile_output
  val profile_format : unit -> Utils.profile_format
  val profile_recursive : unit -> bool
  val new_node : Utils.landmark -> Utils.node
  val get_current_root_node : unit -> Utils.node
  val set_current_root_node : Utils.node -> unit
  val get_current_node_ref : unit -> Utils.node
  val set_current_node_ref : Utils.node -> unit
  val get_cache_miss_ref : unit -> int
  val set_cache_miss_ref : int -> unit
  val incr_cache_miss_ref : unit -> unit
  val get_profiling_stack :
    unit ->
    (Utils.profiling_state, Utils.profiling_state array) Utils.Stack.t
end
