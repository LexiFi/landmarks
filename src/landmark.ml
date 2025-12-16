(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright (C) 2000-2025 LexiFi                                    *)

open Utils

open Landmark_state

external clock: unit -> (Int64.t [@unboxed]) =
  "caml_highres_clock" "caml_highres_clock_native" [@@noalloc]

exception LandmarkFailure of string

module Graph = Graph
module Stack = Utils.Stack

type landmark = Utils.landmark
type counter = Utils.counter
type sampler = Utils.sampler

(** STATE **)

type profile_output = Utils.profile_output =
  | Silent
  | Temporary of string option
  | Channel of out_channel

type textual_option = Utils.textual_option = {threshold : float}

type profile_format = Utils.profile_format =
  | JSON
  | Textual of textual_option

let profiling = profiling

(** REGISTERING **)

let last_landmark_id = ref 1

let landmark_of_id user_id =
  let dummy_key = dummy_key () in
  match W.find_opt (get_landmarks_of_key ()) {dummy_key with key = user_id} with
  | None -> None
  | Some {landmark; _} -> Some landmark

let new_landmark ~key:key_string ~name ~location ~kind () =
  let id = !last_landmark_id in
  incr last_landmark_id;
  let dummy_node = dummy_node () in
  let rec res =
    {
      id;
      name;
      location;
      kind;
      key;
      last_parent = dummy_node;
      last_self = dummy_node;
      last_son = dummy_node;
    }
  and key = { landmark = res; key = key_string} in
  add_landmarks_of_key key;
  res

let new_node = Landmark_state.new_node

let landmark_of_node ({landmark_id = key; name; location; kind; _} : Graph.node) =
  match landmark_of_id key with
  | None -> new_landmark ~key ~name ~kind ~location ()
  | Some landmark -> landmark

let register_generic ~id ~name ~location ~kind () =
  let landmark = new_landmark ~key:id ~name ~location ~kind () in
  if profile_with_debug () then
    Printf.eprintf "[Profiling] registering(%s)\n%!" name;
  landmark

let register_generic ~id ~location kind name =
  match landmark_of_id id with
  | None -> register_generic ~id ~name ~location ~kind ()
  | Some lm -> lm

let register_generic ?id ?location kind name =
  let location =
    match location with
    | Some name -> name
    | None ->
        let callstack = Printexc.get_callstack 5 in
        let backtrace_slots = Printexc.backtrace_slots callstack in
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
    | Some key -> key
    | None -> name^"-"^location
  in
  register_generic ~id ~location kind name

let register ?id ?location name =
  register_generic ?id ?location Graph.Normal name

let register_counter name = register_generic Graph.Counter name

let register_sampler name = register_generic Graph.Sampler name

let reset () =
  reset ()

let () = reset ()

let push_profiling_state () =
  if profile_with_debug () then
    Printf.eprintf "[Profiling] Push profiling state ....\n%!";
  let state =
    let node_info (node: node) =
      let recursive = node.landmark.last_self == node in
      { node; recursive }
    in
    {
      root = get_current_root_node ();
      nodes = List.map node_info (get_allocated_nodes ());
      nodes_len = get_node_id_ref ();
      current = get_current_node_ref ();
      cache_miss = get_cache_miss_ref ();
    }
  in
  clear_cache ();
  set_current_root_node (new_node (landmark_root ()));
  set_current_node_ref (get_current_root_node ());
  set_cache_miss_ref 0;
  set_allocated_nodes [get_current_root_node ()];
  set_node_id_ref 1;
  reset ();
  Stack.push (get_profiling_stack ()) state

let pop_profiling_state () =
  let profiling_stack = get_profiling_stack () in
  if profiling_stack.size > 0 then
    let {root; nodes; nodes_len; current; cache_miss} = Stack.pop profiling_stack in
    set_current_root_node root;
    set_current_node_ref current;
    set_cache_miss_ref cache_miss;
    set_allocated_nodes (List.map (fun {node; recursive} -> if recursive then node.landmark.last_self <- node; node) nodes);
    set_node_id_ref nodes_len

let unroll_until node =
  unroll_until (get_current_node_ref ()) set_current_node_ref node

let landmark_failure msg =
  unroll_until (get_current_root_node ());
  if get_current_node_ref () != get_current_root_node () then
    reset ();
  if profile_with_debug () then
    (Printf.eprintf "Landmark error: %s\n%!" msg; Stdlib.exit 2)
  else
    raise (LandmarkFailure msg)

let get_entering_node ({id;_} as landmark: landmark) =
  let current_node = get_current_node_ref () in
  (* Read the "cache". *)
  if current_node == landmark.last_parent && landmark.last_son != dummy_node () then
    landmark.last_son
  else begin
    incr_cache_miss_ref ();
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
  if profiling () then
    increment ?times counter

let sample sampler x =
  let sampler = get_landmark_body sampler in
  let node = get_entering_node sampler in
  node.calls <- node.calls + 1;
  Stack.push node.distrib x

let sample sampler x =
  if profiling () then
    sample sampler x

let enter landmark =
  let landmark = get_landmark_body landmark in
  let dummy_node = dummy_node () in
  if profile_with_debug () then
    Printf.eprintf "[Profiling] enter%s(%s)\n%!" (if landmark.last_self != dummy_node then " recursive " else "") landmark.name;

  if landmark.last_self == dummy_node || profile_recursive () then begin
    let node = get_entering_node landmark in
    node.calls <- node.calls + 1;
    Stack.push node.fathers (get_current_node_ref ());
    set_current_node_ref node;
    landmark.last_self <- node;
    node.timestamp <- clock ();
    if profile_with_allocated_bytes () then begin
      node.floats.allocated_bytes_stamp <- allocated_bytes ();
      node.floats.allocated_bytes_major_stamp <- allocated_bytes_major ();
    end;
    if profile_with_sys_time () then
      node.floats.sys_timestamp <- Sys.time ()
  end else begin
    let last_self = landmark.last_self in
    last_self.recursive_calls <- last_self.recursive_calls + 1;
    last_self.calls <- last_self.calls + 1
  end

let mismatch_recovering landmark (current_node: node) =
  let expected_landmark = current_node.landmark in
  if expected_landmark != landmark then begin
    let msg =
      Printf.sprintf "landmark failure when closing '%s'<%d> (%s), expecting '%s'<%d> (%s)."
        landmark.name landmark.id landmark.location
        expected_landmark.name landmark.id expected_landmark.location
    in
    Printf.eprintf "Warning: %s\n%!" msg;
    unroll_until landmark.last_self;
    if landmark != (get_current_node_ref ()).landmark then begin
      reset ();
      landmark_failure ("unable to recover from "^msg)
    end
  end

let exit landmark =
  let landmark = get_landmark_body landmark in
  let current_node = get_current_node_ref () in
  if profile_with_debug () then
    Printf.eprintf "[Profiling] exit%s(%s)\n%!" (if landmark.last_self != get_current_node_ref () then " recursive " else "") landmark.name;
  let last_self = landmark.last_self in
  if last_self.recursive_calls = 0 || profile_recursive () then begin
    mismatch_recovering landmark current_node;
    if Stack.size current_node.fathers = 1 then begin
      landmark.last_self <- dummy_node ();
      aggregate_stat_for current_node;
    end;
    set_current_node_ref (get_exiting_node current_node)
  end
  else if not (profile_recursive ()) then
    last_self.recursive_calls <- last_self.recursive_calls - 1

(* These two functions should be inlined. *)
let enter landmark =
  if profiling () then
    enter landmark

let exit landmark =
  if profiling () then
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

type profiling_options = Utils.profiling_options = {
  debug : bool;
  allocated_bytes: bool;
  sys_time : bool;
  recursive : bool;
  output : Utils.profile_output;
  format : Utils.profile_format
}


let default_options = default_options
let set_profiling_options = set_profiling_options
let profiling_options = profiling_options

let start_profiling ?(profiling_options = default_options) () =
  if profiling () then
    failwith "In profiling: it is not allowed to nest profilings.";
  set_profiling_options profiling_options;
  if profile_with_debug () then
    Printf.eprintf "[Profiling] Start profiling %s...\n%!"
      (match profile_with_allocated_bytes (), profile_with_sys_time () with
       | true, true -> "with garbage collection statistics and system time"
       | true, false -> "with garbage collection statistics"
       | false, true -> "with system time"
       | false, false -> "");
  set_profiling true

let rec exit_until_root () =
  let current_node_ref = get_current_node_ref () in
  if current_node_ref != get_current_root_node () then begin
    let landmark = current_node_ref.landmark in
    exit landmark;
    exit_until_root ();
  end

let stop_profiling () =
  if not (profiling ()) then
    failwith "In profiling: cannot stop since profiling is not on-going";
  exit_until_root ();
  let current_node = get_current_node_ref () in
  assert (current_node == get_current_root_node ());
  aggregate_stat_for current_node;
  if profile_with_debug () then
    Printf.eprintf "[Profiling] Stop profiling.\n%!";
  set_profiling false

(** EXPORTING / IMPORTING SLAVE PROFILINGS **)

let rec merge_branch (node:node) graph (imported : Graph.node) =
  let floats = node.floats in
  floats.time <- imported.time +. floats.time;
  floats.sys_time <- imported.sys_time +. floats.sys_time;
  floats.allocated_bytes <- imported.allocated_bytes + floats.allocated_bytes;
  floats.allocated_bytes_major <- imported.allocated_bytes_major + floats.allocated_bytes_major;
  node.calls <- imported.calls + node.calls;
  Float.Array.iter (Stack.push node.distrib) imported.distrib;

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
  Float.Array.iter (Stack.push node.distrib) imported.distrib;
  SparseArray.set parent.children landmark.id node;
  List.iter (new_branch node graph) (Graph.children graph imported)

let merge_aux node graph =
  merge_branch node graph (Graph.root graph)

let merge (graph : Graph.graph) =
  if profile_with_debug () then
    Printf.eprintf "[Profiling] merging foreign graph\n%!";
  merge_aux (get_current_root_node ()) graph

let export ?(label = "") () =
  export ~merge:merge_aux ~label ()

let export_and_reset ?label () =
  let profiling = profiling () in
  if profiling then
    stop_profiling ();
  let res = export ?label () in
  reset ();
  if profiling then
    start_profiling ();
  res

let exit_hook () =
  if profile_with_debug () then
    Printf.eprintf "[Profiling] exit_hook\n%!";
  if profiling () then begin
    stop_profiling ();
    let label =
      String.concat " " (Array.to_list Sys.argv)
    in
    let cg = export ~label () in
    match profile_output (), profile_format () with
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
