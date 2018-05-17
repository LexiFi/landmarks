(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)

open Misc

type id = int

type kind = Normal | Root | Counter | Sampler

let string_of_kind = function
  | Normal -> "normal"
  | Root -> "root"
  | Counter -> "counter"
  | Sampler -> "sampler"

type node = {
  id: int;
  kind : kind;
  landmark_id : int;
  name: string;
  location: string;
  calls: int;
  time: float;
  sons: id list;
  sys_time: float;
  allocated_bytes: float;
  distrib: float array;
}

type graph = {
  nodes : node array;
  label: string;
}

let graph_of_nodes ?(label = "") nodes =
  { nodes = Array.of_list nodes; label }

let sons {nodes; _} node =
  List.map (fun k -> nodes.(k)) node.sons
  |> List.sort (fun n1 n2 -> compare n2.time n1.time)

let nodes {nodes; _} =
  Array.to_list nodes

let root {nodes; _} =
  nodes.(0)

module SetNode = Set.Make(struct
    type t = node
    let compare x y = Pervasives.compare x.id y.id
  end)

module HashNode = Hashtbl.Make (struct
    type t = node
    let equal x y = x.id = y.id
    let hash {id; _} = Hashtbl.hash id
  end)

let path_dfs f g graph =
  let visited_table = HashNode.create 17 in
  let rec aux ancestors_set ancestors vertex =
    let visited = HashNode.mem visited_table vertex in
    if not visited then HashNode.add visited_table vertex ();
    if SetNode.mem vertex ancestors_set then
      g ancestors vertex
    else begin
      f visited ancestors vertex;
      List.iter
        (aux (SetNode.add vertex ancestors_set) (vertex::ancestors))
        (sons graph vertex)
    end
  in
  aux SetNode.empty [] (root graph)

let dfs f g graph =
  let visited_table = HashNode.create 17 in
  let rec aux ancestors_set ancestors vertex =
    let visited = HashNode.mem visited_table vertex in
    if visited then begin
      g ancestors vertex
    end else begin
      HashNode.add visited_table vertex ();
      if f ancestors vertex then
        List.iter
          (aux (SetNode.add vertex ancestors_set) (vertex::ancestors))
          (sons graph vertex)
    end
  in
  aux SetNode.empty [] (root graph)

let depth graph =
  let result = HashNode.create 17 in
  dfs (fun ancestor node ->
      let depth = match ancestor with
        | [] -> 0
        | father :: _ -> (HashNode.find result father) + 1
      in
      begin match HashNode.find result node with
      | exception Not_found -> HashNode.replace result node depth
      | old_depth -> HashNode.replace result node (min old_depth depth)
      end; true)
    (fun _ _ -> ()) graph;
  fun node -> HashNode.find result node

let shallow_ancestor graph =
  let result = HashNode.create 17 in
  dfs
    (fun ancestor node ->
      let sa = match ancestor with
        | [] -> node
        | [ root ] -> root
        | [ father; _ ] -> father
        | father :: _ -> HashNode.find result father
      in
      HashNode.replace result node sa;
      true
    )
    (fun  _ _ -> ()) graph;
  fun node -> HashNode.find result node

let total_number_of_calls graph =
  List.fold_left (fun acc {calls; _ } -> acc + calls) 0 (nodes graph)

let aggregate_landmarks {nodes; label} =
  let group_nodes =
    group_proj (fun ({landmark_id; _} : node) -> landmark_id) (Array.to_list nodes)
  in
  let group_nodes = List.sort (fun l1 l2 -> compare (List.hd l1).landmark_id (List.hd l2).landmark_id) group_nodes in
  let translator = Hashtbl.create 17 in
  List.iteri (fun i l -> Hashtbl.replace translator (List.hd l).landmark_id i) group_nodes;
  let aggregate_nodes l =
    match l with
    | [] -> assert false
    | hd :: tl ->
      let id = Hashtbl.find translator hd.landmark_id in
      let time = List.fold_left (fun acc {time; _} -> acc +. time) hd.time tl in
      let calls = List.fold_left (fun acc {calls; _} -> acc + calls) hd.calls tl in
      let sys_time = List.fold_left (fun acc {sys_time; _} -> acc +. sys_time) hd.sys_time tl in
      let allocated_bytes = List.fold_left (fun acc {allocated_bytes; _} -> acc +. allocated_bytes) hd.allocated_bytes tl in
      let sons =
        let lm_ids_of_sons {sons; _} =
          IntSet.of_list (List.map (fun id -> nodes.(id).landmark_id) sons)
        in
        List.fold_left (fun acc node -> IntSet.union acc (lm_ids_of_sons node)) IntSet.empty l
        |> IntSet.elements
        |> List.map (Hashtbl.find translator)
      in
      { hd with id; time; calls; sys_time; allocated_bytes; sons}
  in
  let nodes = Array.of_list (List.map aggregate_nodes group_nodes) in
  { nodes; label = if label = "" then "aggretated" else label ^ " (aggregated)" }

let intensity ?(proj = fun {time;_} -> time) graph =
  let sa = shallow_ancestor graph in
  fun node ->
    let not_accounted =
      List.fold_left (fun t node -> t -. proj node)
        (proj node) (sons graph node)
    in
    let reference = proj (sa node) in
    if reference = 0.0 then
      0.0
    else
      not_accounted /. reference

let color graph =
  let intensity = intensity graph in
  let red s = "\027[0;31m" ^ s ^ "\027[0m" in
  let bold_red s = "\027[1;31m" ^ s ^ "\027[0m" in
  let yellow s = "\027[0;33m" ^ s ^ "\027[0m" in
  let white s = s in
  let bold_white s = "\027[1;37m" ^ s ^ "\027[0m" in
  let cyan s = "\027[1;36m" ^ s ^ "\027[0m" in
  let purple s = "\027[1;35m" ^ s ^ "\027[0m" in
  let white_bg s = "\027[1;47m" ^ s ^ "\027[0m" in
  let black s = "\027[1;30m" ^ s ^ "\027[0m" in
  fun node ->
    match node.kind with
    | Normal -> begin
        let x = intensity node in
        if x > 0.0 then
          if x > 0.15 then
            bold_red
          else if x > 0.05 then
            red
          else if x > 0.01 then
            yellow
          else
            white
        else
          bold_white
      end
    | Counter -> cyan
    | Sampler -> purple
    | Root -> fun x -> white_bg (black x)

let label graph =
  let nodes =
    group_proj (fun {location; _} -> location)
      (nodes graph)
  in
  let names = flatten_map (fun l ->
      List.sort_uniq Pervasives.compare
        (List.map (fun {name; _} -> name) l)) nodes
  in
  let needs_location =
    StringSet.of_list
      (duplicated_elements names)
  in
  fun {location; name; _}->
    if StringSet.mem name needs_location then
      let location = base_name location in
      Printf.sprintf "%s (%s)" name location
    else name

let output ?(threshold = 1.0) oc graph =
  Printf.fprintf oc "Call graph%s:\n-----------%s\n%!" (if graph.label = "" then "" else " '"^graph.label^"'") (if graph.label = "" then "" else String.make ((String.length graph.label) + 3) '-');
  let label = label graph in
  let color = color graph in
  let human x =
    if x < 1e3 then x, " "
    else if x < 1e6 then x /. 1e3, "K"
    else if x < 1e9 then x /. 1e6, "M"
    else x /. 1e9, "G"
  in
  let spaces depth =
    let bytes = Bytes.make (4*depth + 1) ' ' in
    for k = 1 to depth - 1 do
      Bytes.set bytes (4 * k) '|';
    done;
    Bytes.set bytes (4 * depth) '-';
    Bytes.to_string bytes
  in
  let digits_of_call =
    int_of_float @@
      1. +. log10 (List.map (fun {calls; _} -> calls) (nodes graph)
                   |> List.fold_left max 1
                   |> float_of_int)
  in
  let regular_call ancestors node =
    match ancestors with
    | [] -> true
    | father:: _ ->
      let depth = List.length ancestors in
      let spaces = spaces depth in
      let this_time, father_time = node.time, father.time in
      if node.calls > 0 then
        if father_time > 0.0 then
          let percent = 100.0 *. this_time /. father_time in
          let this_time, unit = human this_time in
          if percent >= threshold then begin
            Printf.fprintf oc "%s\n%!"
              (Printf.sprintf
                 "[ %7.2f%1s cycles in %*d calls ] %s %5.2f%% : %s"
                 this_time unit digits_of_call node.calls spaces percent (color node (label node)));
            true
          end else
            false
        else
          let this_time, unit = human this_time in
          Printf.fprintf oc "%s\n%!"
            (Printf.sprintf
               "[ %7.2f%1s  cycles in %7d calls ] %s * %s"
               this_time unit node.calls spaces (color node (label node)));
          false
       else
         false
  in
  let recursive_call ancestors node =
    let depth = List.length ancestors in
    let spaces = spaces depth in
    Printf.fprintf oc "%37s%s*** RECURSIVE CALL TO '%s' ***\n%!" "!!!!" spaces (label node)
  in
  dfs regular_call recursive_call graph;
  let aggregated_graph = aggregate_landmarks graph in
  let all_nodes =
    List.sort
      (fun {time = time1; _} {time = time2; _} -> compare time2 time1)
      (nodes aggregated_graph)
  in
  let normal_nodes = List.filter (fun n -> n.kind = Normal || n.kind = Root) all_nodes in
  let sample_nodes = List.filter (fun n -> n.kind = Sampler) all_nodes in
  let profile_with_sys_time = List.exists (fun {sys_time; _} -> sys_time <> 0.0) normal_nodes in
  let profile_with_allocated_bytes = List.exists (fun {allocated_bytes; _} -> allocated_bytes <> 0.0) normal_nodes in
  let optional_headers =
    match profile_with_sys_time, profile_with_allocated_bytes with
    | true, true -> Printf.sprintf "; %8s; %8s" "Sys time" "Allocated bytes"
    | true, false -> Printf.sprintf "; %8s" "Sys time"
    | false, true -> Printf.sprintf "; %8s" "Allocated bytes"
    | false, false -> ""
  in
  let optional_columns sys_time allocated_bytes =
    match profile_with_sys_time, profile_with_allocated_bytes with
    | true, true -> Printf.sprintf "; %8.3f; %8.0f" sys_time allocated_bytes
    | true, false -> Printf.sprintf "; %8.3f" sys_time
    | false, true -> Printf.sprintf "; %8.0f" allocated_bytes
    | false, false -> ""
  in
  if threshold > 0.0 then
    Printf.fprintf oc "\nNote: Nodes accounting for less than %2.2f%% of their parent have been ignored.\n%!" threshold;
  Printf.fprintf oc "\nAggregated table:\n----------------\n%!";
  let max_name_length = List.fold_left (fun acc {name; _} -> max acc (String.length name)) 0 normal_nodes in
  let max_location_length = List.fold_left (fun acc {location; _} -> max acc (String.length location)) 0 normal_nodes in
  Printf.fprintf oc "%*s; %*s; %8s; %8s%s\n%!"
    max_name_length "Name" max_location_length "Filename" "Calls" "Time" optional_headers;
  let print_row ({name; location; calls;
                  time; allocated_bytes; sys_time; _}) =
    let time, unit = human time in
    Printf.fprintf oc "%*s; %*s; %8d; %7.2f%1s%s\n%!"
      max_name_length name max_location_length location calls time unit (optional_columns sys_time allocated_bytes)
  in
  List.iter print_row normal_nodes;
  if sample_nodes <> [] then begin
    Printf.fprintf oc "\nSamplings\n----------\n%!";
    let stats d =
      let avg = (Array.fold_left (+.) 0.0 d) /. (float (Array.length d)) in
      let square x = x *. x in
      let stddev =
        sqrt ((Array.fold_left (fun acc x -> acc +. square (x -. avg)) 0.0 d) /. (float (Array.length d)))
      in
      avg, stddev
    in
    List.iter (fun node ->
        let avg, stddev = stats node.distrib in
        Printf.fprintf oc "%s: avg = %g, stddev = %g\n%!" (label node) avg stddev
      )
      sample_nodes;
  end

module JSON = struct

type json =
  | String of string
  | Int of int
  | Float of float
  | Map of (string * json) list
  | List of json list
  | ListClosure of int * (int -> json)

open Format

let rec output oc = function
  | String s ->
    fprintf oc "\"%s\"" (String.escaped s)
  | Int n ->
    fprintf oc "%d" n
  | Float f ->
    fprintf oc "%f" f
  | Map l ->
    fprintf oc "{@,";
    let first = ref true in
    List.iter (fun (name, json) ->
        if !first then
          first := false
        else
          fprintf oc ",@,";
        fprintf oc "@[<v 2>%S: %a@]" name output json
    ) l;
    fprintf oc "@;<0 -2>}"
  | List [] -> fprintf oc "[]"
  | List [x] -> fprintf oc "[%a]" output x
  | List l when List.for_all (function Int _ -> true | _ -> false) l ->
    fprintf oc "[%s]" (String.concat ", " (List.map (function Int x -> string_of_int x | _ -> assert false) l))
  | List l ->
    fprintf oc "[@,";
    let first = ref true in
    List.iter (fun json ->
        if !first then
          first := false
        else
          fprintf oc ",@,";
        fprintf oc "@[<v 2>%a@]" output json
    ) l;
    fprintf oc "@;<0 -2>]"

  | ListClosure (n,f) ->
    fprintf oc "[@,";
    for k = 0 to n - 1 do
      let json = f k in
      if k > 0 then
        fprintf oc ",@,";
      fprintf oc "@[<v 2>%a@]" output json
    done;
    fprintf oc "@;<0 -2>]"

let output oc =
  fprintf (formatter_of_out_channel oc) "@[<v 2>%a@]@." output

end

open JSON

let json_of_node
    {id; kind; landmark_id; name; location;
     calls; time; sons; sys_time; allocated_bytes; distrib} =
  Map [ "id", Int id;
        "kind", String (string_of_kind kind);
        "landmark_id", Int landmark_id;
        "name", String name;
        "location", String location;
        "calls", Int calls;
        "time", Float time;
        "sons", List (List.map (fun x -> Int x) sons);
        "sys_time", Float sys_time;
        "allocated_bytes", Float allocated_bytes;
        "distrib", List (List.map (fun x -> Float x) (Array.to_list distrib)) ]

let json_of_graphs {nodes; label} =
  Map ["nodes", ListClosure (Array.length nodes, fun k -> json_of_node nodes.(k));
       "label", String label]

let output_json oc graph = JSON.output oc (json_of_graphs graph)
