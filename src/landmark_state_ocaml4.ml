(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright (C) 2000-2025 LexiFi                                    *)

open Utils

module Stack = Utils.Stack

type landmark = landmark_body

type t = unit

let landmark_of_landmark_body () l = l

let new_node_ref: (t -> landmark_body -> node) ref =
  ref (fun _ _ -> failwith "uninitialized function \"new_node_ref\"")
let export_ref: (t -> string -> Graph.graph) ref =
  ref (fun _ -> failwith "uninitialized function \"export_ref\"")
let reset_state_ref: (t -> unit) ref =
  ref (fun _ -> failwith "uninitialized function \"reset_state_ref\"")
let stop_profiling_ref: (t -> unit) ref =
  ref (fun _ -> failwith "uninitialized function \"stop_profiling_ref\"")
let iter_registered_landmarks: ((landmark -> unit) -> unit) ref =
  ref (fun _ -> ())
let get_state () = ()

let rec landmark_root = {
  kind = Graph.Root;
  id = 0;
  name = "ROOT";
  location = __FILE__;
  key = "";
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

let dummy_landmark () = landmark_root

let clear_cache () =
  !iter_registered_landmarks (
    fun landmark ->
      landmark.last_son <- dummy_node;
      landmark.last_parent <- dummy_node;
      landmark.last_self <- dummy_node;
  )

let landmark_root () = landmark_root
let dummy_node () = dummy_node

let profiling_ref = ref false
let profiling () = !profiling_ref
let set_profiling () b =  profiling_ref := b
let get_ds_landmark () l = l

let node_id_ref = ref 0
let get_node_id_ref () = !node_id_ref
let set_node_id_ref () n = node_id_ref := n

let allocated_nodes = ref []
let get_allocated_nodes () = !allocated_nodes
let set_allocated_nodes () l = allocated_nodes := l

let get_incr_node_id_ref () =
  let id = !node_id_ref in
  incr node_id_ref;
  id

let current_root_node = ref (dummy_node ())
let current_node_ref = ref !current_root_node

let get_current_root_node, get_current_node_ref =
  let init = ref false in
  let set_nodes () =
    current_root_node := !new_node_ref () (landmark_root ());
    current_node_ref := !current_root_node;
    init := true
  in
  let get_current_root_node () =
    if not !init then (
      set_nodes ();
      !current_root_node)
    else !current_root_node
  in
  let get_current_node_ref () =
    if not !init
    then (
      current_root_node := !new_node_ref () (landmark_root ());
      current_node_ref := !current_root_node;
      !current_node_ref
    )
    else !current_node_ref
  in
  get_current_root_node, get_current_node_ref

let set_current_root_node () node =
  current_root_node := node

let set_current_node_ref () node =
  current_node_ref := node

let cache_miss_ref = ref 0
let get_cache_miss_ref () = !cache_miss_ref
let set_cache_miss_ref () n = cache_miss_ref := n

let profiling_stack =
  let dummy = {
    root = dummy_node ();
    current = dummy_node ();
    nodes = [{node = dummy_node (); recursive = false}];
    cache_miss = 0;
    nodes_len = 1}
  in
  Stack.make Array dummy 7
let incr_cache_miss_ref () = incr cache_miss_ref
let get_profiling_stack () = profiling_stack

let export ~merge:_ ?(label = "") () =
  !export_ref () label
