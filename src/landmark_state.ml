(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright (C) 2000-2025 LexiFi                                    *)

open Utils

module Stack = Utils.Stack

module Ref = struct

  let profiling_ref = ref false
  let profiling () = !profiling_ref
  let set_profiling b =  profiling_ref := b


  let last_landmark_id = ref 1
  let get_last_landmark_id () = !last_landmark_id
  let incr_last_landmark_id () = incr last_landmark_id

  let landmarks_of_key = W.create 17
  let get_landmarks_of_key () = landmarks_of_key
  let add_landmarks_of_key key = W.add landmarks_of_key key

  let node_id_ref = ref 0
  let get_node_id_ref () = !node_id_ref
  let set_node_id_ref n = node_id_ref := n

  let allocated_nodes = ref []
  let get_allocated_nodes () = !allocated_nodes
  let set_allocated_nodes l = allocated_nodes := l

  let profile_with_debug = ref false
  let profile_with_allocated_bytes = ref false
  let profile_with_sys_time = ref false
  let profile_output = ref Silent
  let profile_format = ref (Textual {threshold = 1.0})
  let profile_recursive = ref false

  let set_profiling_options {debug; allocated_bytes; sys_time; output; format; recursive} =
    profile_with_debug := debug;
    profile_with_allocated_bytes := allocated_bytes;
    profile_with_sys_time := sys_time;
    profile_output := output;
    profile_format := format;
    profile_recursive := recursive

  let profiling_options () = {
    debug = !profile_with_debug;
    allocated_bytes = !profile_with_allocated_bytes;
    sys_time = !profile_with_sys_time;
    recursive = !profile_recursive;
    output = !profile_output;
    format = !profile_format
  }

  let profile_with_debug () = !profile_with_debug
  let profile_with_allocated_bytes () = !profile_with_allocated_bytes
  let profile_with_sys_time () = !profile_with_sys_time
  let profile_output () = !profile_output
  let profile_format () = !profile_format
  let profile_recursive () = !profile_recursive

  let new_node landmark =
    new_node landmark (profile_with_debug ()) node_id_ref allocated_nodes

  let current_root_node = ref (new_node landmark_root)
  let get_current_root_node () = !current_root_node
  let set_current_root_node node = current_root_node := node

  let current_node_ref = ref !current_root_node
  let get_current_node_ref () = !current_node_ref
  let set_current_node_ref node = current_node_ref := node

  let cache_miss_ref = ref 0
  let get_cache_miss_ref () = !cache_miss_ref
  let set_cache_miss_ref n = cache_miss_ref := n

  let profiling_stack =
    let dummy =
      {root = dummy_node; current = dummy_node; nodes = [{node = dummy_node; recursive = false}]; cache_miss = 0; nodes_len = 1}
    in
    Stack.make Array dummy 7
  let incr_cache_miss_ref () = incr cache_miss_ref
  let get_profiling_stack () = profiling_stack

end
