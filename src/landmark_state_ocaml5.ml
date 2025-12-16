(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright (C) 2000-2025 LexiFi                                    *)

open Utils

module Stack = Utils.Stack

let init_dummies () =
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
  in
  landmark_root, dummy_node, dummy_key

let dummies = Domain.DLS.new_key init_dummies

let landmark_root () =
  let landmark_root, _, _ = Domain.DLS.get dummies in
  landmark_root

let dummy_node () =
  let _, dummy_node, _ = Domain.DLS.get dummies in
  dummy_node

let dummy_key () =
  let _, _, dummy_key = Domain.DLS.get dummies in
  dummy_key

type nodes = {
  mutable node_id_ref: int;
  mutable allocated_nodes: node list;
}

let init_nodes () = {
  node_id_ref = 0;
  allocated_nodes = [];
}

let get_incr_node_id_ref nodes () =
  let id = nodes.node_id_ref in
  nodes.node_id_ref <- id + 1;
  id

let add_allocated_node nodes node =
  nodes.allocated_nodes <- node :: nodes.allocated_nodes

type state = {
  nodes: nodes;

  mutable profiling_ref : bool;
  landmarks_of_key: W.t;
  mutable cache_miss_ref: int;
  profiling_stack: (profiling_state, profiling_state array) Stack.t;

  mutable current_root_node : node;
  mutable current_node_ref : node;

  mutable child_states : state list;
  (* The states of child domains spawned by the main one *)
  mutable graph: Graph.graph;
  (* Used by child states to store their own graphs *)

  mutable registered: bool;
}

let clear_cache gls =
  let dummy_node = dummy_node () in
  W.iter (
    fun {landmark; _} ->
      landmark.last_son <- dummy_node;
      landmark.last_parent <- dummy_node;
      landmark.last_self <- dummy_node;
  ) (gls.landmarks_of_key)

let reset_aux gls =
  if profile_with_debug () then
    Printf.eprintf "[Profiling] resetting ...\n%!";
  let current_root_node = gls.current_root_node in
  let floats = current_root_node.floats in
  floats.time <- 0.0;
  floats.allocated_bytes <- 0;
  floats.sys_time <- 0.0;
  current_root_node.calls <- 0;
  current_root_node.recursive_calls <- 0;
  stamp_root current_root_node;
  SparseArray.reset current_root_node.children;
  gls.nodes.allocated_nodes <- [current_root_node];
  gls.current_node_ref <- current_root_node;
  gls.cache_miss_ref <- 0;
  (* TODO: ensure that the dummy node of gls is used *)
  clear_cache gls;
  gls.nodes.node_id_ref <- 1

let init_state () =
  let nodes = init_nodes () in
  let rootnode =
    new_node (landmark_root ()) (dummy_node ()) false
      (get_incr_node_id_ref nodes) (add_allocated_node nodes);
  in {
    nodes;

    profiling_ref = false;
    landmarks_of_key = W.create 17;
    cache_miss_ref = 0;
    profiling_stack = (
      let dummy =
        {root = dummy_node (); current = dummy_node (); nodes = [{node = dummy_node (); recursive = false}]; cache_miss = 0; nodes_len = 1}
      in
      Stack.make Array dummy 7
    );

    current_root_node = rootnode;
    current_node_ref = rootnode;
    child_states = [];
    graph = {nodes = [||]; label = ""; root = 0 };
    registered = false;
  }

let copy_landmark_cache (w: W.t) =
  let w' = W.create 17 in
  let dummy_node = dummy_node () in
  (* TODO: incorrent since it uses the dummy nodes of the parent domain *)
  W.iter (
    fun key ->
      W.add w' {
        key with
        landmark = {
          key.landmark with
          last_parent = dummy_node;
          last_son = dummy_node;
          last_self = dummy_node} }
  ) w;
  w'

let state =
  Domain.DLS.new_key
    ~split_from_parent:(fun s ->
        let child_state = init_state () in
        let child_state =
          { child_state with
            profiling_ref = s.profiling_ref;
            landmarks_of_key = copy_landmark_cache s.landmarks_of_key }
        in
        s.child_states <- child_state :: s.child_states;
        child_state.profiling_ref <- s.profiling_ref;
        reset_aux child_state;
        child_state
      )
    init_state


(* Adapted copies of landmark.ml functions  *)
let mismatch_recovering st landmark =
  let current_node = st.current_node_ref in
  let expected_landmark = current_node.landmark in
  if expected_landmark != landmark then begin
    let msg =
      Printf.sprintf "landmark failure when closing '%s'<%d> (%s), expecting '%s'<%d> (%s)."
        landmark.name landmark.id landmark.location
        expected_landmark.name landmark.id expected_landmark.location
    in
    Printf.eprintf "Warning: %s\n%!" msg;
    unroll_until current_node
      (fun node -> st.current_node_ref <- node)
      landmark.last_self;
    if landmark != st.current_node_ref.landmark then begin
      reset_aux st;
      failwith ("unable to recover from "^msg)
    end
  end

let get_exiting_node st =
  if Stack.size st.current_node_ref.fathers = 0 then
    failwith "Stack underflow"
  else
    Stack.pop st.current_node_ref.fathers

let exit st =
  let landmark = (* get_landmark_body *) st.current_node_ref.landmark in
  let current_node = st.current_node_ref in
  let last_self = landmark.last_self in
  if last_self.recursive_calls = 0 || profile_recursive () then begin
    mismatch_recovering st landmark;
    if Stack.size current_node.fathers = 1 then begin
      landmark.last_self <- dummy_node ();
      aggregate_stat_for current_node;
    end;
    st.current_node_ref <- (get_exiting_node st)
  end
  else if not (profile_recursive ()) then
    last_self.recursive_calls <- last_self.recursive_calls - 1

let exit_until_root st =
  let rec aux () =
    if st.current_node_ref != st.current_root_node then begin
      exit st;
      aux ();
    end
  in
  aux ()

let stop_profiling_aux st =
  assert (st.current_node_ref == st.current_root_node);
  if st.profiling_ref then (
    exit_until_root st;
    assert (st.current_node_ref == st.current_root_node);
    aggregate_stat_for st.current_node_ref;
    if profile_with_debug () then
      Printf.eprintf "[Profiling] Stop profiling.\n%!";
    st.profiling_ref <- false
  )

let export state label =
  if state.profiling_ref then begin
    aggregate_stat_for state.current_root_node;
    stamp_root state.current_root_node
  end;
  let all_nodes = List.rev state.nodes.allocated_nodes in
  let nodes = array_list_map export_node all_nodes in
  {Graph.nodes; label; root = 0}

let get_state () =
  let st = Domain.DLS.get state in
  if not st.registered && not (Domain.is_main_domain ()) then (
    Domain.at_exit (fun () ->
        stop_profiling_aux st;
        st.graph <- export st ""
      );
    st.registered <- true;
  );
  st

let profiling () = (get_state ()).profiling_ref
let set_profiling b = (get_state ()).profiling_ref <- b
let get_landmarks_of_key =
  let initialized = Domain.DLS.new_key (fun () -> false) in
  fun () ->
    let landmarks_of_key = (get_state ()).landmarks_of_key in
    if not (Domain.DLS.get initialized) then (
      Domain.DLS.set initialized true;
      let dummy_node = dummy_node () in
      W.iter (
        fun key ->
          key.landmark.last_parent <- dummy_node;
          key.landmark.last_son <- dummy_node;
          key.landmark.last_self <- dummy_node
      ) landmarks_of_key
    );
    landmarks_of_key

let add_landmarks_of_key key = W.add (get_state ()).landmarks_of_key key

let get_landmark_body (l: landmark) =
  let dummy_key = dummy_key () in
  let { landmark; _ } =
    W.find (get_landmarks_of_key ()) { dummy_key with key = l.key.key }
  in
  landmark

let get_node_id_ref () = (get_state ()).nodes.node_id_ref
let set_node_id_ref n = (get_state ()).nodes.node_id_ref <- n
let get_allocated_nodes () = (get_state ()).nodes.allocated_nodes
let set_allocated_nodes l = (get_state ()).nodes.allocated_nodes <- l

let new_node landmark =
  let { nodes; _ } = get_state () in
  new_node landmark (dummy_node ()) (profile_with_debug ())
    (get_incr_node_id_ref nodes) (add_allocated_node nodes)


let get_current_root_node () = (get_state ()).current_root_node
let set_current_root_node (node: node) =
  (get_state ()).current_root_node <- node

let get_current_node_ref () = (get_state ()).current_node_ref
let set_current_node_ref (node: node) =
  (get_state ()).current_node_ref <- node

let get_cache_miss_ref () = (get_state ()).cache_miss_ref
let set_cache_miss_ref n = (get_state ()).cache_miss_ref <- n
let incr_cache_miss_ref () =
  let state = get_state () in
  state.cache_miss_ref <- state.cache_miss_ref + 1
let get_profiling_stack () = (get_state ()).profiling_stack


let reset () =
  reset_aux (get_state ())

let clear_cache () =
  clear_cache (get_state ())

let export ~merge ?(label = "") () =
  let state = get_state () in
  List.iter (
    fun st ->
      merge state.current_root_node st.graph
  ) state.child_states;
  export state label
