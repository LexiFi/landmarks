(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright (C) 2000-2025 LexiFi                                    *)

open Utils

module Stack = Utils.Stack

type landmark = landmark_body Lazy.t Domain.DLS.key
(* _ Lazy.t is used to ensure that the landmark instance of a child domain is
   created in runtime of the child domain. *)

type nodes = {
  mutable node_id_ref: int;
  mutable allocated_nodes: node list;
}

type t = {
  landmark_root: landmark_body;
  dummy_node : node;

  nodes: nodes;

  mutable profiling_ref : bool;
  mutable cache_miss_ref: int;
  profiling_stack: (profiling_state, profiling_state array) Stack.t;

  mutable current_root_node : node;
  mutable current_node_ref : node;

  mutable child_states : t list;
  (* The states of child domains spawned by the main one *)
  mutable graph: Graph.graph;
  (* Used by child states to store their own graphs *)

  mutable registered: bool;
}

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

let init_nodes () = {
  node_id_ref = 0;
  allocated_nodes = [];
}

let get_incr_node_id_ref st =
  let id = st.nodes.node_id_ref in
  st.nodes.node_id_ref <- id + 1;
  id

let init_state () =
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
  in
  let nodes = init_nodes () in
  let st = {
    landmark_root;
    dummy_node;
    nodes;
    profiling_ref = false;
    cache_miss_ref = 0;
    profiling_stack = (
      let dummy = {
        root = dummy_node;
        current = dummy_node;
        nodes = [{node = dummy_node; recursive = false}];
        cache_miss = 0;
        nodes_len = 1}
      in
      Stack.make Array dummy 7);
    child_states = [];
    graph = {nodes = [||]; label = ""; root = 0 };
    registered = false;
    (* Temprory *)
    current_root_node = dummy_node;
    current_node_ref = dummy_node;
  }
  in
  let root_node = !new_node_ref st landmark_root in
  { st with current_root_node = root_node; current_node_ref = root_node }

let state =
  Domain.DLS.new_key
    ~split_from_parent:(fun s ->
        let child_state = init_state () in
        let child_state =
          { child_state with
            profiling_ref = s.profiling_ref }
        in
        s.child_states <- child_state :: s.child_states;
        child_state.profiling_ref <- s.profiling_ref;
        !reset_state_ref child_state;
        child_state
      )
    init_state

let get_state () =
  let st = Domain.DLS.get state in
  if not st.registered && not (Domain.is_main_domain ()) then (
    Domain.at_exit (fun () ->
        !stop_profiling_ref st;
        st.graph <- !export_ref st ""
      );
    st.registered <- true;
  );
  st

let dummy_landmark st = Domain.DLS.new_key (fun () -> lazy st.landmark_root)
(* Only used for search in the weak HashSet, will never be accessed *)

let landmark_root st = st.landmark_root
let dummy_node st = st.dummy_node

let get_ds_landmark _st (l: landmark) = Lazy.force (Domain.DLS.get l)

let landmark_of_landmark_body _st (l: landmark_body): landmark =
  Domain.DLS.new_key
    ~split_from_parent:(
      fun l ->
        let { id; name; location; kind; key; _ } = Lazy.force l in
        lazy (
          let st = get_state () in {
            id;
            name;
            location;
            kind;
            key;
            last_parent = dummy_node st;
            last_self = dummy_node st;
            last_son = dummy_node st;
          }))
    (fun () -> lazy l)

let clear_cache gls =
  !iter_registered_landmarks (
    fun landmark ->
      let landmark = get_ds_landmark () landmark in
      landmark.last_son <- gls.dummy_node;
      landmark.last_parent <- gls.dummy_node;
      landmark.last_self <- gls.dummy_node;
  )

let profiling st = st.profiling_ref
let set_profiling st b = st.profiling_ref <- b

let get_node_id_ref st = st.nodes.node_id_ref
let set_node_id_ref st n = st.nodes.node_id_ref <- n
let get_allocated_nodes st = st.nodes.allocated_nodes
let set_allocated_nodes st l = st.nodes.allocated_nodes <- l

let get_current_root_node st = st.current_root_node
let set_current_root_node st (node: node) =
  st.current_root_node <- node

let get_current_node_ref st = st.current_node_ref
let set_current_node_ref st (node: node) =
  st.current_node_ref <- node

let get_cache_miss_ref st = st.cache_miss_ref
let set_cache_miss_ref st n = st.cache_miss_ref <- n
let incr_cache_miss_ref st = st.cache_miss_ref <- st.cache_miss_ref + 1
let get_profiling_stack st = st.profiling_stack

let rec merge_child_state_graphs ~merge state =
  List.iter (
    fun st ->
      merge_child_state_graphs ~merge st;
      merge state.current_root_node st.graph
  ) state.child_states

let export ~merge ?(label = "") state =
  merge_child_state_graphs ~merge state;
  !export_ref state label
