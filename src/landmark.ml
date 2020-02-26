(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)

external clock: unit -> Int64.t = "caml_highres_clock"

exception LandmarkFailure of string

module Graph = Graph

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
  location: string;
  user_id: string option;

  mutable last_parent: node;
  mutable last_son: node;
  mutable last_self: node;
}

and node = {
  landmark: landmark;

  id: int;

  children: node SparseArray.t;
  fathers: node Stack.t;

  mutable calls: int;
  mutable recursive_calls: int;
  mutable timestamp: Int64.t;
  distrib: float Stack.t;
  floats : floats;
}

and floats = {
  mutable time: float;
  mutable allocated_bytes: float;
  mutable allocated_bytes_stamp: float;
  mutable sys_time: float;
  mutable sys_timestamp: float;
}

and counter = landmark

and sampler = landmark

let new_floats () = {
  time = 0.0;
  allocated_bytes = 0.0;
  allocated_bytes_stamp = 0.0;
  sys_time = 0.0;
  sys_timestamp = 0.0
}

let rec landmark_root = {
  kind = Graph.Root;
  id = 0;
  name = "ROOT";
  location = __FILE__;
  user_id = None;
  last_parent = dummy_node;
  last_son = dummy_node;
  last_self = dummy_node;
}

and dummy_node = {
  landmark = landmark_root;
  id = 0;
  children = SparseArray.dummy ();
  fathers = Stack.dummy ();
  floats = new_floats ();
  calls = 0;
  recursive_calls = 0;
  distrib = Stack.dummy ();
  timestamp = Int64.zero
}

(** STATE **)

type profile_output =
  | Silent
  | Temporary of string option
  | Channel of out_channel

type textual_option = {threshold : float}

type profile_format =
  | JSON
  | Textual of textual_option

let profiling_ref = ref false
let profile_with_debug = ref false
let profile_with_allocated_bytes = ref false
let profile_with_sys_time = ref false
let profile_output = ref Silent
let profile_format = ref (Textual {threshold = 1.0})
let profile_recursive = ref false

let profiling () = !profiling_ref

(** REGISTERING **)

let last_landmark_id = ref 1
let landmarks_of_id = Hashtbl.create 17
let landmarks_of_user_id = Hashtbl.create 17

let new_landmark ?user_id ~name ~location ~kind () =
  let id = !last_landmark_id in
  incr last_landmark_id;
  let res =
    {
      id;
      name;
      location;
      kind;
      user_id;
      last_parent = dummy_node;
      last_self = dummy_node;
      last_son = dummy_node;
    }
  in
  Hashtbl.add landmarks_of_id id res;
  begin match user_id with
  | Some user_id ->
      Hashtbl.add landmarks_of_user_id user_id res;
  | None -> ()
  end;
  res

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
    children = SparseArray.make dummy_node 7;

    calls = 0;
    recursive_calls = 0;
    timestamp = Int64.zero;
    floats = new_floats ();
  } in
  allocated_nodes := node :: !allocated_nodes;
  node

let current_root_node = ref (new_node landmark_root)

let registered_landmarks = ref [landmark_root]

let landmark_of_node ({landmark_id = id; name; location; kind; _} : Graph.node) =
  if String.length id > 0 && id.[0] = '#' then
    let user_id = String.sub id 1 (String.length id - 1) in
    match Hashtbl.find landmarks_of_user_id user_id with
    | exception Not_found -> new_landmark ~user_id ~name ~kind ~location ()
    | landmark -> landmark
  else
    match int_of_string id with
    | exception _ ->
        Printf.eprintf "[PROFILING] Cannot parse landmark id:'%s'\n%!" id;
        new_landmark ~name ~kind ~location ()
    | id ->
        match Hashtbl.find landmarks_of_id id with
        | exception Not_found ->
            Printf.eprintf
              "[PROFILING] Inconsistency: the landmark id:'%d', name:'%s', location:'%s' \
               has not been registered in the master process.\n%!" id name location;
            new_landmark ~name ~kind ~location ()
        | landmark ->
            if landmark.name <> name
            || landmark.location <> location then begin
              Printf.eprintf
                "[PROFILING] Inconsistency: the 'master' landmark '%s' ('%s') \
                 has the same id (%d) than the 'worker' landmark '%s' ('%s')\n%!"
                landmark.name landmark.location landmark.id name location;
              new_landmark ~name ~kind ~location ()
            end else
              landmark

let register_generic ~id ~name ~location ~kind () =
  let landmark = new_landmark ~user_id:id ~name ~location ~kind () in
  registered_landmarks := landmark :: !registered_landmarks;
  if !profile_with_debug then
    Printf.eprintf "[Profiling] registering(%s)\n%!" name;
  landmark

let register_generic ~id ~location kind name =
  match Hashtbl.find landmarks_of_user_id id with
  | exception Not_found ->
      register_generic ~id ~name ~location ~kind ()
  | lm -> lm

let register_generic ?id ?location kind name call_stack =
  let location =
    match location with
    | Some name -> name
    | None ->
        let backtrace_slots = Printexc.backtrace_slots call_stack in
        match backtrace_slots with
        | Some [||] | None -> "unknown"
        | Some slots ->
            let last_slot = slots.(Array.length slots - 1) in
            match Printexc.Slot.location last_slot with
            | Some {Printexc.filename; line_number; _} ->
                Printf.sprintf "%s:%d" filename line_number
            | None -> "internal"
  in
  let id =
    match id with
    | Some id -> id
    | None -> name^"-"^location
  in
  register_generic ~id ~location kind name

let register ?id ?location name =
  let call_stack = Printexc.get_callstack 4 in
  register_generic ?id ?location Graph.Normal name call_stack

let register_counter name =
  let call_stack = Printexc.get_callstack 4 in
  register_generic Graph.Counter name call_stack

let register_sampler name =
  let call_stack = Printexc.get_callstack 4 in
  register_generic Graph.Sampler name call_stack

let current_node_ref = ref !current_root_node
let cache_miss_ref = ref 0

let stamp_root () =
  !current_root_node.timestamp <- clock ();
  if !profile_with_allocated_bytes then
    !current_root_node.floats.allocated_bytes <- Gc.allocated_bytes ();
  if !profile_with_sys_time then
    !current_root_node.floats.sys_time <- Sys.time ()

let clear_cache () =
  let reset_landmark landmark =
    landmark.last_son <- dummy_node;
    landmark.last_parent <- dummy_node;
    landmark.last_self <- dummy_node;
  in
  List.iter reset_landmark !registered_landmarks

type profiling_state = {
  root : node;
  nodes: node list;
  nodes_len: int;
  current: node;
  cache_miss: int
}

let profiling_stack =
  let dummy =
    {root = dummy_node; current = dummy_node; nodes = [dummy_node]; cache_miss = 0; nodes_len = 1}
  in
  Stack.make dummy 7

let push_profiling_state () =
  let state =
    {
      root = !current_root_node;
      nodes = !allocated_nodes;
      nodes_len = !node_id_ref;
      current = !current_node_ref;
      cache_miss = !cache_miss_ref;
    }
  in
  clear_cache ();
  current_root_node := new_node landmark_root;
  current_node_ref := !current_root_node;
  cache_miss_ref := 0;
  allocated_nodes := [!current_root_node];
  node_id_ref := 1;
  Stack.push profiling_stack state

let pop_profiling_state () =
  if profiling_stack.size > 0 then
    let {root; nodes; nodes_len; current; cache_miss} = Stack.pop profiling_stack in
    current_root_node := root;
    current_node_ref := current;
    cache_miss_ref := cache_miss;
    allocated_nodes := nodes;
    node_id_ref := nodes_len

let reset () =
  if !profile_with_debug then
    Printf.eprintf "[Profiling] resetting ...\n%!";
  (* reset dummy_node *)
  let floats = !current_root_node.floats in
  floats.time <- 0.0;
  floats.allocated_bytes <- 0.0;
  floats.sys_time <- 0.0;
  !current_root_node.calls <- 0;
  !current_root_node.recursive_calls <- 0;
  stamp_root ();
  SparseArray.reset !current_root_node.children;
  allocated_nodes := [!current_root_node];
  current_node_ref := !current_root_node;
  cache_miss_ref := 0;
  clear_cache ();
  node_id_ref := 1

let () = reset ()

let unroll_until node =
  while
    let current_node = !current_node_ref in
    current_node != node
    && Stack.size current_node.fathers > 0
    && (current_node_ref := Stack.pop current_node.fathers; true)
  do () done

let landmark_failure msg =
  unroll_until !current_root_node;
  if !current_node_ref != !current_root_node then
    reset ();
  if !profile_with_debug then
    (Printf.eprintf "Landmark error: %s\n%!" msg; Stdlib.exit 2)
  else
    raise (LandmarkFailure msg)

let get_entering_node ({id;_} as landmark) =
  let current_node = !current_node_ref in
  (* Read the "cache". *)
  if current_node == landmark.last_parent && landmark.last_son != dummy_node then
    landmark.last_son
  else begin
    incr cache_miss_ref;
    (* We fetch the son or create it. *)
    let children = current_node.children in
    let son = try
        SparseArray.get children id
      with Not_found ->
        let son = new_node landmark in
        SparseArray.set current_node.children id son;
        son
    in
    (* Fill the "cache". *)
    landmark.last_parent <- current_node;
    landmark.last_son <- son;
    son
  end

let get_exiting_node current_node =
  if Stack.size current_node.fathers = 0 then
    landmark_failure "Stack underflow"
  else
    Stack.pop current_node.fathers

let increment ?(times = 1) counter =
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
    Printf.eprintf "[Profiling] enter%s(%s)\n%!" (if landmark.last_self != dummy_node then " recursive " else "") landmark.name;

  if landmark.last_self == dummy_node || !profile_recursive then begin
    let node = get_entering_node landmark in
    node.calls <- node.calls + 1;
    Stack.push node.fathers !current_node_ref;
    current_node_ref := node;
    landmark.last_self <- node;
    node.timestamp <- clock ();
    if !profile_with_allocated_bytes then
      node.floats.allocated_bytes_stamp <- Gc.allocated_bytes ();
    if !profile_with_sys_time then
      node.floats.sys_timestamp <- Sys.time ()
  end else begin
    let last_self = landmark.last_self in
    last_self.recursive_calls <- last_self.recursive_calls + 1;
    last_self.calls <- last_self.calls + 1
  end

let mismatch_recovering landmark current_node =
  let expected_landmark = current_node.landmark in
  if expected_landmark != landmark then begin
    let msg =
      Printf.sprintf "landmark failure when closing '%s' (%s), expecting '%s' (%s)."
        landmark.name landmark.location
        expected_landmark.name expected_landmark.location
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
  if !profile_with_allocated_bytes then
    floats.allocated_bytes <- floats.allocated_bytes
                              +. ((Gc.allocated_bytes ()) -. floats.allocated_bytes_stamp);
  if !profile_with_sys_time then
    floats.sys_time <- floats.sys_time
                       +. (Sys.time () -. floats.sys_timestamp)

let exit landmark =
  if !profile_with_debug then
    Printf.eprintf "[Profiling] exit%s(%s)\n%!" (if landmark.last_self != !current_node_ref then " recursive " else "") landmark.name;
  let current_node = !current_node_ref in
  let last_self = landmark.last_self in
  if last_self.recursive_calls = 0 || !profile_recursive then begin
    mismatch_recovering landmark current_node;
    if Stack.size current_node.fathers = 1 then begin
      landmark.last_self <- dummy_node;
      aggregate_stat_for current_node;
    end;
    current_node_ref := get_exiting_node current_node
  end
  else if not !profile_recursive then
    last_self.recursive_calls <- last_self.recursive_calls - 1

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
  allocated_bytes: bool;
  sys_time : bool;
  recursive : bool;
  output : profile_output;
  format : profile_format
}

let default_options = {
  debug = false;
  allocated_bytes = true;
  sys_time = false;
  recursive = false;
  output = Channel stderr;
  format = Textual {threshold = 1.0};
}

let set_profiling_options {debug; allocated_bytes; sys_time; output; format; recursive} =
  profile_with_allocated_bytes := allocated_bytes;
  profile_with_sys_time := sys_time;
  profile_with_debug := debug;
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

let start_profiling ?(profiling_options = default_options) () =
  if !profiling_ref then
    failwith "In profiling: it is not allowed to nest profilings.";
  set_profiling_options profiling_options;
  if !profile_with_debug then
    Printf.eprintf "[Profiling] Start profiling %s...\n%!"
      (match !profile_with_allocated_bytes, !profile_with_sys_time with
       | true, true -> "with garbage collection statistics and system time"
       | true, false -> "with garbage collection statistics"
       | false, true -> "with system time"
       | false, false -> "");
  profiling_ref := true

let rec exit_until_root () =
  if !current_node_ref != !current_root_node then begin
    let landmark = !current_node_ref.landmark in
    exit landmark;
    exit_until_root ();
  end

let stop_profiling () =
  if not !profiling_ref then
    failwith "In profiling: cannot stop since profiling is not on-going";
  exit_until_root ();
  let current_node = !current_node_ref in
  assert (current_node == !current_root_node);
  aggregate_stat_for current_node;
  if !profile_with_debug then
    Printf.eprintf "[Profiling] Stop profiling.\n%!";
  profiling_ref := false

(** EXPORTING / IMPORTING SLAVE PROFILINGS **)

let array_list_map f l =
  let size = List.length l in
  match l with
  | [] -> [||]
  | hd :: tl ->
      let res = Array.make size (f hd) in
      List.iteri (fun k x -> res.(k+1) <- f x) tl; res

let export ?(label = "") () =
  let export_node {landmark; id; calls; floats; children; distrib; _} =
    let {id = landmark_id; user_id; name; location; kind; _} = landmark in
    let {time; allocated_bytes; sys_time; _} = floats in
    let children =
      List.map (fun ({id;_} : node) -> id) (SparseArray.values children)
    in
    let landmark_id =
      match user_id with
      | None -> string_of_int landmark_id
      | Some user_id -> "#"^user_id
    in
    {Graph.landmark_id; id; name; location; calls; time; kind;
     allocated_bytes; sys_time; children; distrib = Stack.to_array distrib}
  in
  if !profiling_ref then begin
    aggregate_stat_for !current_root_node;
    stamp_root ()
  end;
  let all_nodes = List.rev !allocated_nodes in
  let nodes = array_list_map export_node all_nodes in
  {Graph.nodes; label; root = 0}

let export_and_reset ?label () =
  let profiling = !profiling_ref in
  if profiling then
    stop_profiling ();
  let res = export ?label () in
  reset ();
  if profiling then
    start_profiling ();
  res

let rec merge_branch node graph (imported : Graph.node) =
  let floats = node.floats in
  floats.time <- imported.time +. floats.time;
  floats.sys_time <- imported.sys_time +. floats.sys_time;
  floats.allocated_bytes <- imported.allocated_bytes +. floats.allocated_bytes;
  node.calls <- imported.calls + node.calls;
  Array.iter (Stack.push node.distrib) imported.distrib;

  let children = Graph.children graph imported in
  List.iter
    (fun (imported_son : Graph.node) ->
       let landmark = landmark_of_node imported_son in
       match SparseArray.get node.children landmark.id with
       | exception Not_found ->
           new_branch node graph imported_son
       | son -> merge_branch son graph imported_son
    ) children

and new_branch parent graph (imported : Graph.node) =
  let landmark = landmark_of_node imported in
  let node = new_node landmark in
  node.calls <- imported.calls;
  let floats = node.floats in
  floats.time <- imported.time;
  floats.allocated_bytes <- imported.allocated_bytes;
  floats.sys_time <- imported.sys_time;
  Array.iter (Stack.push node.distrib) imported.distrib;
  SparseArray.set parent.children landmark.id node;
  List.iter (new_branch node graph) (Graph.children graph imported)

let merge (graph : Graph.graph) =
  if !profile_with_debug then
    Printf.eprintf "[Profiling] merging foreign graph\n%!";
  merge_branch !current_root_node graph (Graph.root graph)

let exit_hook () =
  if !profile_with_debug then
    Printf.eprintf "[Profiling] exit_hook\n%!";
  if !profiling_ref then begin
    stop_profiling ();
    let label =
      String.concat " " (Array.to_list Sys.argv)
    in
    let cg = export ~label () in
    match !profile_output, !profile_format with
    | Silent, _ -> ()
    | Channel out, Textual {threshold} ->
        Graph.output ~threshold out cg
    | Channel out, JSON ->
        Graph.output_json out cg
    | Temporary temp_dir, format ->
        let tmp_file, oc =
          Filename.open_temp_file ?temp_dir "profile_at_exit" ".tmp"
        in
        Printf.eprintf
          "[Profiling] Dumping profiling information in file '%s'.\n" tmp_file;
        flush stdout;
        (match format with
         | Textual {threshold} -> Graph.output ~threshold oc cg
         | JSON -> Graph.output_json oc cg);
        close_out oc
  end

let () = Stdlib.at_exit exit_hook


let parse_env_options s =
  let open Printf in
  let debug = ref false in
  let format = ref (Textual {threshold = 1.0}) in
  let output = ref (Channel stderr) in
  let sys_time = ref false in
  let recursive = ref false in
  let allocated_bytes = ref false in
  let split_trim c s =
    List.map String.trim (Misc.split c s)
  in
  let warning s =
    eprintf "[LANDMARKS] %s.\n%!" s
  in
  let parse_option s =
    let invalid_for opt given =
      warning (sprintf
                 "The argument '%s' in not valid for the option '%s'" given opt)
    in
    let expect_no_argument opt =
      warning (sprintf "The option '%s' expects no argument" opt)
    in
    match split_trim '=' s with
    | [] -> ()
    | ["debug"] -> debug := true
    | "debug" :: _  -> expect_no_argument "debug"
    | [ "threshold" ; percent ] ->
        begin match !format with
        | Textual _ ->
            let threshold = try Some (float_of_string percent) with _ -> None in
            begin match threshold with
            | None ->
                warning (Printf.sprintf "Unable to parse threshold '%s'" percent)
            | Some threshold ->
                format := Textual {threshold}
            end
        | _ -> warning (Printf.sprintf "The option threshold only makes sense with the 'textual' format.")
        end
    | [ "format"; "textual" ] ->
        begin match !format with
        | Textual _ -> ()
        | _ -> format := Textual {threshold = 1.0};
        end
    | [ "format"; "json" ] -> format := JSON;
    | [ "format"; unknown ] -> invalid_for "format" unknown
    | [ "output"; "stderr" ] -> output := Channel stderr
    | [ "output"; "stdout" ] -> output := Channel stdout
    | [ "output"; temporary ] when Misc.starts_with ~prefix:"temporary" temporary ->
        begin match split_trim ':' temporary with
        | ["temporary"] -> output := Temporary None
        | ["temporary"; dir_spec] ->
            begin match split_trim '"' dir_spec with
            | [""; dir; ""] -> output := Temporary (Some dir)
            | [dir] -> output := Temporary (Some dir)
            | _ -> invalid_for "output" temporary
            end
        | _ -> invalid_for "output" temporary
        end
    | [ "output"; file_spec ] ->
        (match split_trim '"' file_spec with
         | [""; file; ""] | [file] ->
             (try
                output := Channel (open_out file)
              with _ -> warning (sprintf "Unable to open '%s'" file))
         | _ -> invalid_for "output" file_spec)
    | ["time"] -> sys_time := true
    | "time" :: _  -> expect_no_argument "time"
    | ["recursive"] -> recursive := true
    | "recursive" :: _  -> expect_no_argument "recursive"
    | ["allocation"] -> allocated_bytes := true
    | "allocation" :: _ -> expect_no_argument "allocation"
    | ["off"] -> raise Exit
    | "off" :: _ -> expect_no_argument "off"
    | ["auto"] | ["remove"] | ["threads"] -> () (* read by the ppx extension *)
    | "auto" :: _  -> expect_no_argument "auto"
    | "remove" :: _ -> expect_no_argument "remove"
    | "threads" :: _  -> expect_no_argument "threads"
    | [""] | ["on"] | ["1"] -> ()
    | opt :: _ :: _ -> warning (Printf.sprintf "To many '=' after '%s'" opt)
    | unknown :: _ -> warning (sprintf "Unknown option '%s'" unknown)
  in
  List.iter parse_option (split_trim ',' s);
  {debug = !debug; allocated_bytes = !allocated_bytes; sys_time = !sys_time;
   output = !output; format = !format; recursive = !recursive}

let () = match Sys.getenv "OCAML_LANDMARKS" with
  | exception Not_found -> ()
  | str ->
      try start_profiling ~profiling_options:(parse_env_options str) ()
      with Exit -> ()

external raise : exn -> 'a = "%raise"
