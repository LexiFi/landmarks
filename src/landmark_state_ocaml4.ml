(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright (C) 2000-2025 LexiFi                                    *)

open Utils

module Stack = Utils.Stack

let rec landmark_root = {
  kind = Graph.Root;
  id = 0;
  name = "ROOT";
  location = __FILE__;
  key = { key = ""; landmark = landmark_root};
  last_parent = dummy_node;
  last_son = dummy_node;
  last_self = dummy_node;
}

and dummy_node = {
  landmark = landmark_root;
  id = 0;
  children = SparseArray.dummy ();
  fathers = Stack.dummy Array;
  floats = new_floats ();
  calls = 0;
  recursive_calls = 0;
  distrib = Stack.dummy Float;
  timestamp = Int64.zero
}

and dummy_key = { key = ""; landmark = landmark_root}

let landmark_root () = landmark_root
let dummy_node () = dummy_node
let dummy_key () = dummy_key

let profiling_ref = ref false
let profiling () = !profiling_ref
let set_profiling b =  profiling_ref := b

let landmarks_of_key = W.create 17
let get_landmarks_of_key () = landmarks_of_key
let add_landmarks_of_key key = W.add landmarks_of_key key
let get_landmark_body l = l

let node_id_ref = ref 0
let get_node_id_ref () = !node_id_ref
let set_node_id_ref n = node_id_ref := n

let allocated_nodes = ref []
let get_allocated_nodes () = !allocated_nodes
let set_allocated_nodes l = allocated_nodes := l

let get_incr_node_id_ref () =
  let id = !node_id_ref in
  incr node_id_ref;
  id

let add_allocated_node node =
  allocated_nodes := node :: !allocated_nodes

let new_node landmark =
  new_node landmark (dummy_node ()) (profile_with_debug ()) get_incr_node_id_ref add_allocated_node

let current_root_node = ref (new_node (landmark_root ()))
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
    {root = dummy_node (); current = dummy_node (); nodes = [{node = dummy_node (); recursive = false}]; cache_miss = 0; nodes_len = 1}
  in
  Stack.make Array dummy 7
let incr_cache_miss_ref () = incr cache_miss_ref
let get_profiling_stack () = profiling_stack

let clear_cache () =
  W.iter (
    fun {landmark; _} ->
      landmark.last_son <- dummy_node ();
      landmark.last_parent <- dummy_node ();
      landmark.last_self <- dummy_node ();
  ) (get_landmarks_of_key ())

let reset () =
  if profile_with_debug () then
    Printf.eprintf "[Profiling] resetting ...\n%!";
  (* reset dummy_node *)
  let current_root_node = get_current_root_node () in
  let floats = current_root_node.floats in
  floats.time <- 0.0;
  floats.allocated_bytes <- 0;
  floats.sys_time <- 0.0;
  current_root_node.calls <- 0;
  current_root_node.recursive_calls <- 0;
  stamp_root current_root_node;
  SparseArray.reset current_root_node.children;
  set_allocated_nodes [current_root_node];
  set_current_node_ref current_root_node;
  set_cache_miss_ref 0;
  clear_cache ();
  set_node_id_ref 1

let export ~merge:_ ?(label = "") () =
  if profiling () then begin
    aggregate_stat_for (get_current_root_node ());
    stamp_root (get_current_root_node ())
  end;
  let all_nodes = List.rev (get_allocated_nodes ()) in
  let nodes = array_list_map export_node all_nodes in
  {Graph.nodes; label; root = 0}
