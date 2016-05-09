(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)

external clock: unit -> Int64.t = "caml_highres_clock"

exception LandmarkFailure of string

module Graph = Landmark_graph


module SparseArray = struct
  type 'a t = {
    mutable keys : int array;
    mutable data : 'a array;
    mutable size : int;
  }

  (* /!\ Dummy cannot be resized. *)
  let dummy () = { keys = [||]; data = [||]; size = 0 }

  let make null n =
    let n = max n 1 in
    {
      keys = Array.make n 0;
      data = Array.make n null;
      size = 0;
    }

  let reset sparse_array = sparse_array.size <- 0

  let get {keys; data; size} id =
    let min = ref 0 in
    let max = ref (size - 1) in
    while !min < !max do
      let middle = (!min + !max) / 2 in
      if Array.unsafe_get keys middle < id then
        min := middle + 1
      else
        max := middle
    done;
    let idx = !min in
    if idx = !max &&
       Array.unsafe_get keys idx = id then
      Array.unsafe_get data idx
    else
      raise Not_found

  let swap a i j =
    let t = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- t

  let values {data; size; _} =
    let result = ref [] in
    for k = 0 to size-1 do
      result := data.(k) :: !result;
    done;
    List.rev !result

  let bubble {keys; data; size} =
    let pos = ref size in
    let key = keys.(size) in
    while
      let p = !pos in
      let q = p - 1 in
      if key < keys.(q) then begin
        swap keys p q;
        swap data p q;
        pos := q;
        q > 0
      end else false
    do () done

  let is_full ({keys; size; _}) = Array.length keys = size

  let resize ({keys; data; size} as sparse_array) =
    if is_full sparse_array then begin
      assert (size > 0);
      let new_length = (2 * (size + 1)) - 1 in
      sparse_array.keys <- Array.make new_length 0;
      sparse_array.data <- Array.make new_length sparse_array.data.(0);
      Array.blit keys 0 sparse_array.keys 0 size;
      Array.blit data 0 sparse_array.data 0 size;
    end

  let set sparse_array id node =
    resize sparse_array;
    let size = sparse_array.size in
    sparse_array.keys.(size) <- id;
    sparse_array.data.(size) <- node;
    if size > 0 then
      bubble sparse_array;
    sparse_array.size <- sparse_array.size + 1
end

module Stack = struct
  type 'a t = {
    mutable data : 'a array;
    mutable size : int
  }
  (* /!\ Dummy cannot be resized. *)
  let dummy () = { data = [||]; size = 0 }
  let make null n = { data = Array.make (max 1 n) null; size = 0 }
  let size {size; _} = size
  let resize ({size; data} as stack) =
    if size = Array.length data then begin
      assert (size > 0);
      let new_length = (2 * (size + 1)) - 1 in
      stack.data <- Array.make new_length data.(0);
      Array.blit data 0 stack.data 0 size;
    end

  let push ({size; _} as stack) x =
    resize stack;
    stack.data.(size) <- x;
    stack.size <- size + 1

  let pop stack =
    stack.size <- stack.size - 1;
    stack.data.(stack.size)

  let to_array {data; size; _} = Array.sub data 0 size
end


type landmark = {
    id: int;
    kind : Graph.kind;
    name: string;
    filename: string;

    mutable last_parent: node;
    mutable last_son: node;
    mutable last_self: node;
  }

and node = {
  landmark: landmark;

  id: int;

  sons: node SparseArray.t;
  fathers: node Stack.t;

  mutable calls: int;
  mutable timestamp: Int64.t;
  distrib: float Stack.t;
  floats : floats;
}

and floats = {
    mutable time: float;
    mutable gc_stat: float;
    mutable gc_statstamp: float;
    mutable sys_time: float;
    mutable sys_timestamp: float;
}

and counter = landmark

and sampler = landmark

let new_floats () = {
  time = 0.0;
  gc_stat = 0.0;
  gc_statstamp = 0.0;
  sys_time = 0.0;
  sys_timestamp = 0.0
}

let rec landmark_root = {
    kind = Graph.Root;
    id = 0;
    name = "ROOT";
    filename = __FILE__;
    last_parent = dummy_node;
    last_son = dummy_node;
    last_self = dummy_node;
}

and dummy_node = {
    landmark = landmark_root;
    id = 0;
    sons = SparseArray.dummy ();
    fathers = Stack.dummy ();
    floats = new_floats ();
    calls = 0;
    distrib = Stack.dummy ();
    timestamp = Int64.zero
}

(** STATE **)

type profile_output =
  | Silent
  | Temporary
  | Channel of out_channel

type profile_format =
  | JSON
  | Textual

let profiling_ref = ref false

let profile_with_debug = ref false
let profile_with_gc_stat = ref false
let profile_with_sys_time = ref false
let profile_output = ref Silent
let profile_format = ref Textual

let profiling () = !profiling_ref


(** REGISTERING **)

let last_landmark_id = ref 1
let new_landmark name filename kind =
  let id = !last_landmark_id in
  incr last_landmark_id;
  {
    id;
    name;
    filename;
    kind;
    last_parent = dummy_node;
    last_self = dummy_node;
    last_son = dummy_node;
  }


let node_id_ref = ref 0
let allocated_nodes = ref []
let new_node landmark =
  if !profile_with_debug then
    Printf.eprintf "[Profiling] Allocating new node for %s...\n%!" landmark.name;
  let id = !node_id_ref in
  incr node_id_ref;
  let node = {
    landmark;
    id;

    fathers = Stack.make dummy_node 1;
    distrib = Stack.make 0.0 0;
    sons = SparseArray.make dummy_node 7;

    calls = 0;
    timestamp = Int64.zero;
    floats = new_floats ();
  } in
  allocated_nodes := node :: !allocated_nodes;
  node

let root_node = new_node landmark_root

let registered_landmarks = ref [landmark_root]

let landmark_of_id id =
  List.nth !registered_landmarks
    ((List.length !registered_landmarks) - (id + 1))

let register_generic ?filename kind name call_stack =
  let filename =
    match filename with
    | Some name -> name
    | None ->
      let backtrace_slots = Printexc.backtrace_slots call_stack in
      match backtrace_slots with
      | Some slots when Array.length slots >= 2 ->
        let loc = Printexc.Slot.location slots.(1) in
        (match loc with
         | Some loc -> loc.Printexc.filename
         | None -> "internal")
      | _ -> "unknown"
  in
  let landmark = new_landmark name filename kind in
  if List.exists (fun landmark ->
      name = landmark.name && filename = landmark.filename)
      !registered_landmarks then
    failwith
      (Printf.sprintf
         "The landmark '%s' is registered twice in '%s'." name filename);
  registered_landmarks := landmark :: !registered_landmarks;
  if !profile_with_debug then
    Printf.eprintf "[Profiling] registering(%s)\n%!" name;
  landmark

let register ?filename name =
  let call_stack = Printexc.get_callstack 3 in
  register_generic ?filename Graph.Normal name call_stack

let register_counter name =
  let call_stack = Printexc.get_callstack 3 in
  register_generic Graph.Counter name call_stack

let register_sampler name =
  let call_stack = Printexc.get_callstack 3 in
  register_generic Graph.Sampler name call_stack

let current_node_ref = ref root_node
let cache_miss_ref = ref 0

let reset () =
  if !profile_with_debug then
    Printf.eprintf "[Profiling] resetting ...\n%!";
  (* reset dummy_node *)
  let floats = root_node.floats in
  floats.time <- 0.0;
  floats.gc_stat <- 0.0;
  floats.sys_time <- 0.0;
  root_node.calls <- 0;
  root_node.timestamp <- clock ();
  SparseArray.reset root_node.sons;
  allocated_nodes := [root_node];
  current_node_ref := root_node;
  cache_miss_ref := 0;
  let reset_landmark landmark =
    landmark.last_son <- dummy_node;
    landmark.last_parent <- dummy_node;
    landmark.last_self <- dummy_node;
  in
  List.iter reset_landmark !registered_landmarks;
  node_id_ref := 1

let unroll_until node =
  while
    let current_node = !current_node_ref in
       current_node != node
    && Stack.size current_node.fathers > 0
    && (current_node_ref := Stack.pop current_node.fathers; true)
  do () done

let landmark_failure msg =
  unroll_until root_node;
  if !current_node_ref != root_node then
    reset ();
  if !profile_with_debug then
    (Printf.eprintf "%s\n%!" msg; Pervasives.exit 2)
  else
    raise (LandmarkFailure msg)

let get_entering_node ({id;_} as landmark) =
  let current_node = !current_node_ref in
  (* Read the "cache". *)
  if current_node == landmark.last_parent && landmark.last_son != dummy_node then
    landmark.last_son
  else begin
    (* Detects a recursive call and creates a cycle in the graph. *)
    let last_self = landmark.last_self in
    if last_self != dummy_node then
      last_self
    else begin
      incr cache_miss_ref;
      (* We fetch the son or create it. *)
      let sons = current_node.sons in
      let son = try
          SparseArray.get sons id
        with Not_found ->
          let son = new_node landmark in
          SparseArray.set current_node.sons id son;
          son
      in
      (* Fill the "cache". *)
      landmark.last_parent <- current_node;
      landmark.last_son <- son;
      son
    end
  end

let get_exiting_node current_node =
  if Stack.size current_node.fathers = 0 then
    landmark_failure "Stack underflow"
  else
    Stack.pop current_node.fathers

let increment ?(times = 0) counter =
  let node = get_entering_node counter in
  node.calls <- node.calls + times

let increment ?times counter =
  if !profiling_ref then
    increment ?times counter

let sample sampler x =
  let node = get_entering_node sampler in
  node.calls <- node.calls + 1;
  Stack.push node.distrib x

let sample sampler x =
  if !profiling_ref then
    sample sampler x

let enter landmark =
  if !profile_with_debug then
    Printf.eprintf "[Profiling] enter(%s)\n%!" landmark.name;

  let node = get_entering_node landmark in

  node.calls <- node.calls + 1;
  Stack.push node.fathers !current_node_ref;
  current_node_ref := node;

  (* "Stamps" are only collected when it is not a recursive call. *)
  if landmark.last_self == dummy_node then begin
    landmark.last_self <- node;
    node.timestamp <- clock ();
    if !profile_with_gc_stat then
      node.floats.gc_statstamp <- Gc.allocated_bytes ();
    if !profile_with_sys_time then
      node.floats.sys_timestamp <- Sys.time ();
  end

let mismatch_recovering landmark current_node =
  let expected_landmark = current_node.landmark in
  if expected_landmark != landmark then begin
    let msg =
      Printf.sprintf "landmark failure when closing '%s', expecting '%s'."
                                      landmark.name expected_landmark.name
    in
    Printf.eprintf "Warning: %s\n%!" msg;
    unroll_until landmark.last_self;
    if landmark != !current_node_ref.landmark then begin
      reset ();
      landmark_failure ("unable to recover from "^msg)
    end
  end

let aggregate_stat_for current_node =
  let floats = current_node.floats in
  floats.time <- floats.time
                 +. Int64.(to_float (sub (clock ()) current_node.timestamp));
  if !profile_with_gc_stat then
    floats.gc_stat <- floats.gc_stat
                 +. ((Gc.allocated_bytes ()) -. floats.gc_statstamp);
  if !profile_with_sys_time then
    floats.sys_time <- floats.sys_time
                 +. (Sys.time () -. floats.sys_timestamp)

let exit landmark =
  if !profile_with_debug then
    Printf.eprintf "[Profiling] exit(%s)\n%!" landmark.name;
  let current_node = !current_node_ref in

  mismatch_recovering landmark current_node;
  if Stack.size current_node.fathers = 1 then begin
    landmark.last_self <- dummy_node;
    aggregate_stat_for current_node;
  end;
  current_node_ref := get_exiting_node current_node

(* These two functions should be inlined. *)
let enter landmark =
  if !profiling_ref then
    enter landmark

let exit landmark =
  if !profiling_ref then
    exit landmark

(** HELPERS **)

let wrap node f x =
  enter node;
  try
    let res = f x in
    exit node;
    res
  with LandmarkFailure _ as e -> raise e
     | e -> exit node; raise e

let unsafe_wrap node f x =
  enter node;
  let res = f x in
  exit node;
  res

(** PROFILERS **)

type profiling_options = {
  debug : bool;
  gc_stat: bool;
  sys_time : bool;
  output : profile_output;
  format : profile_format
}

let default_options = {
  debug = false;
  gc_stat = true;
  sys_time = false;
  output = Channel stderr;
  format = Textual;
}

let set_profiling_options {debug; gc_stat; sys_time; output; format} =
  profile_with_gc_stat := gc_stat;
  profile_with_sys_time := sys_time;
  profile_with_debug := debug;
  profile_output := output;
  profile_format := format


let start_profiling ?(profiling_options = default_options) () =
  if !profiling_ref then
    failwith "In profiling: it is not allowed to nest profilings.";
  set_profiling_options profiling_options;
  if !profile_with_debug then
    Printf.eprintf "[Profiling] Start profiling %s...\n%!"
      (match !profile_with_gc_stat, !profile_with_sys_time with
       | true, true -> "with garbage collection statistics and system time"
       | true, false -> "with garbage collection statistics"
       | false, true -> "with system time"
       | false, false -> "");
  profiling_ref := true;
  reset ()


let stop_profiling () =
  if not !profiling_ref then
    failwith "In profiling: cannot stop since profiling is not on-going";
  let current_node = !current_node_ref in
  if current_node != root_node then
    landmark_failure
      (Printf.sprintf
         "The landmark '%s' is still opened at the end of profiling."
         current_node.landmark.name);
  aggregate_stat_for current_node;
  if !profile_with_debug then
    Printf.eprintf "[Profiling] Stop profiling.\n%!";
  profiling_ref := false


(** EXPORTING / IMPORTING SLAVE PROFILINGS **)


let export () =
  let export_node {landmark; id; calls; floats; sons; distrib; _} =
    let {id = landmark_id; name; filename; kind; _} = landmark in
    let {time; gc_stat; sys_time; _} = floats in
    let sons =
      List.map (fun ({id;_} : node) -> id) (SparseArray.values sons)
    in
    {Graph.landmark_id; id; name; filename; calls; time; kind;
     gc_stat; sys_time; sons; distrib = Stack.to_array distrib}
  in
  if !current_node_ref != root_node then
    (let msg = Printf.sprintf
         "Export of call graph is only allowed when all landmarks have been \
          exited ('%s' is still open)" !current_node_ref.landmark.name
     in
     failwith msg);
  let clock = clock () in
  root_node.floats.time <-
    root_node.floats.time +.
    (Int64.to_float (Int64.sub clock root_node.timestamp));
  root_node.timestamp <- clock;
  let all_nodes = List.rev !allocated_nodes in
  Graph.graph_of_nodes (List.map export_node all_nodes)

let export_and_reset () =
  let profiling = !profiling_ref in
  if profiling then
    stop_profiling ();
  let res = export () in
  reset ();
  if profiling then
    start_profiling ();
  res

let rec merge_branch node graph (imported : Graph.node) =
  let floats = node.floats in
  floats.time <- imported.time +. floats.time;
  floats.sys_time <- imported.sys_time +. floats.sys_time;
  floats.gc_stat <- imported.gc_stat +. floats.gc_stat;
  node.calls <- imported.calls + node.calls;
  Array.iter (Stack.push node.distrib) imported.distrib;

  let sons = Graph.sons graph imported in
  List.iter (fun (imported_son : Graph.node) ->
      match SparseArray.get node.sons imported_son.landmark_id with
      | exception Not_found ->
        new_branch node graph imported_son
      | son -> merge_branch son graph imported_son) sons

and new_branch parent graph ({landmark_id; _} as imported : Graph.node) =
  let landmark =
    match landmark_of_id landmark_id with
    | exception Not_found ->
      let msg = Printf.sprintf
          "%sThe landmark with id %d has not been registered in master process."
          inconsistency_msg landmark_id
      in
      failwith msg
    | x -> x
  in
  check_landmark landmark imported;
  let node = new_node landmark in
  node.calls <- imported.calls;
  let floats = node.floats in
  floats.time <- imported.time;
  floats.gc_stat <- imported.gc_stat;
  floats.sys_time <- imported.sys_time;
  Array.iter (Stack.push node.distrib) imported.distrib;
  SparseArray.set parent.sons landmark_id node;
  List.iter (new_branch node graph) (Graph.sons graph imported);

and inconsistency_msg =
 "Inconsistency while importing profiling information of slaves processes:\n"

and check_landmark landmark imported =
  if landmark.name <> imported.name
  || landmark.filename <> imported.filename then
    let msg =
      Printf.sprintf
        "%sThe 'master' landmark '%s' ('%s') has the same id (%d) than the \
         'slave' landmark'%s' ('%s')"
        inconsistency_msg landmark.name landmark.filename landmark.id
        imported.name imported.filename
    in
    failwith msg


let merge (graph : Graph.graph) =
  if !profile_with_debug then
    Printf.eprintf "[Profiling] merging foreign graph\n%!";
  merge_branch root_node graph (Graph.root graph)

let exit_hook () =
  if !profile_with_debug then
    Printf.eprintf "[Profiling] exit_hook\n%!";
  if !profiling_ref then begin
    stop_profiling ();
    let cg = export () in
    match !profile_output, !profile_format with
    | Silent, _ -> ()
    | Channel out, Textual ->
      Graph.output out cg
    | Channel out, JSON ->
      Graph.output_json out cg
    | Temporary, format ->
      let tmp_file, oc = Filename.open_temp_file "profile_at_exit" ".tmp" in
      Printf.printf
        "[Profiling] Dumping profiling information in file '%s'.\n" tmp_file;
      flush stdout;
      (match format with
      | Textual -> Graph.output oc cg
      | JSON -> Graph.output_json oc cg);
      close_out oc
  end

let () = Pervasives.at_exit exit_hook
