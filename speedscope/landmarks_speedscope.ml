(*
   Export to the Speedscope format
*)

open Landmark

let schema_url = "https://www.speedscope.app/file-format-schema.json"
let exporter_name = "landmarks"

let parse_location loc =
  match String.rindex_opt loc ':' with
  | None -> loc, None
  | Some i ->
      let file = String.sub loc 0 i in
      let rest = String.sub loc (i + 1) (String.length loc - i - 1) in
      (match int_of_string_opt rest with
       | Some n -> file, Some n
       | None   -> loc, None)

(* One Speedscope frame per unique landmark (by landmark_id), skipping Root. *)
let make_frames (graph : Graph.graph) =
  let tbl = Hashtbl.create 16 in
  let frames = ref [] in
  let next_idx = ref 0 in
  Array.iter (fun (node : Graph.node) ->
      if node.kind <> Graph.Root && not (Hashtbl.mem tbl node.landmark_id) then begin
        let file, line = parse_location node.location in
        let frame = Speedscope_fmt.create_frame ~name:node.name ~file ?line () in
        Hashtbl.add tbl node.landmark_id !next_idx;
        frames := frame :: !frames;
        incr next_idx
      end
    ) graph.nodes;
  List.rev !frames, tbl

(* DFS producing one sample per call-graph node with positive self-time.
   Each sample is a stack of frame indices from outermost to innermost
   caller (Speedscope's "bottom to top" convention).
   Counter and Sampler nodes are skipped. *)
let collect_samples ~use_sys_time (graph : Graph.graph) frame_idx =
  let samples = ref [] in
  let weights = ref [] in
  let visited = Hashtbl.create 16 in
  let node_time (n : Graph.node) = if use_sys_time then n.sys_time else n.time in
  let rec aux stack (node : Graph.node) =
    if not (Hashtbl.mem visited node.id) then begin
      Hashtbl.add visited node.id ();
      match node.kind with
      | Graph.Root ->
          List.iter (aux stack) (Graph.children graph node)
      | Graph.Counter | Graph.Sampler -> ()
      | Graph.Normal ->
          let fidx = Hashtbl.find frame_idx node.landmark_id in
          let stack' = fidx :: stack in    (* maintained reversed; reversed on emit *)
          let child_list = Graph.children graph node in
          let child_time =
            List.fold_left (fun acc c -> acc +. node_time c) 0.0 child_list
          in
          let self_time = node_time node -. child_time in
          if self_time > 0.0 then begin
            samples := List.rev stack' :: !samples;
            weights := self_time :: !weights
          end;
          List.iter (aux stack') child_list
    end
  in
  aux [] (Graph.root graph);
  List.rev !samples, List.rev !weights

let exporter oc (graph : Graph.graph) =
  let frames, frame_idx = make_frames graph in
  let use_sys_time =
    Array.exists (fun (n : Graph.node) -> n.sys_time > 0.0) graph.nodes
  in
  let samples, weights = collect_samples ~use_sys_time graph frame_idx in
  let end_value = List.fold_left ( +. ) 0.0 weights in
  let weight_unit =
    if use_sys_time then Speedscope_fmt.Seconds else Speedscope_fmt.None_
  in
  let profile = Speedscope_fmt.create_sampled_profile
      ~type_:"sampled"
      ~name:graph.label
      ~unit:weight_unit
      ~start_value:0.0
      ~end_value
      ~samples
      ~weights
      ()
  in
  let shared = Speedscope_fmt.create_profile_shared ~frames () in
  let file = Speedscope_fmt.create_file_format
      ~schema:schema_url
      ?name:(if graph.label = "" then None else Some graph.label)
      ~exporter:exporter_name
      ~profiles:[profile]
      ~shared
      ()
  in
  Yojson.Safe.pretty_to_channel ~std:true oc
    (Speedscope_fmt.yojson_of_file_format file);
  output_char oc '\n'

(* This relies on the [-linkall] flag passed with [-a] when building
   the library to ensure the registration takes place. *)
let () = Landmark.register_exporter "speedscope" exporter
