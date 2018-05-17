(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)

open Js_core

let error s =
  Js_core.alert ("Error: "^s);
  failwith s

let document = Window.document window

module Helper = struct
  let removeAll element =
    while
      match Node.last_child element with
      | Some child -> Node.remove_child element child; true
      | None -> false
    do () done

  let element_of_id id =
    match Document.get_element_by_id document id with
    | Some element -> element
    | None -> error (Printf.sprintf "Element of id '%s' not found" id)

  let input_of_id id =
    match Html.retype (element_of_id id) with
    | `Input input -> input
    | _ ->
      error (Printf.sprintf "Element of id '%s' should be an input element." id)

  let hide element =
    Element.set_attribute element "style" "display: none"

  let show element =
    Element.remove_attribute element "style"

  let tabs_logic l = if l = [] then () else
      let tabs, contents = List.split l in
      let tabs = Array.of_list tabs in
      let contents = Array.of_list contents in
      let size = Array.length contents in
      let activate k =
        Element.set_class_name tabs.(k) "active";
        show contents.(k);
        for i = 0 to size - 1 do
          if i <> k then begin
            Element.set_class_name tabs.(i) "";
            hide contents.(i);
          end
        done;
      in
      activate 0;
      for k = 0 to size - 1 do
        Element.set_onclick tabs.(k) (fun () -> activate k);
      done

  let rec sortable_table cols rows inside =
    let open Document in
    removeAll inside;
    let table = create_html_table document in
    Node.append_child inside table;
    let thead = create_html_thead document in
    Node.append_child table thead;
    let tbody = create_html_tbody document in
    Node.append_child table tbody;
    let first_row = create_html_tr document in
    Node.append_child thead first_row;
    List.iter (fun (header, cmp, _) ->
        let th = create_html_th document in
        Element.set_onclick th (fun () ->
            let rows = List.sort cmp rows in
            let cols =
              List.map (fun (header, cmp, proj) ->
                  (header, (fun x y -> cmp y x), proj)) cols
            in
            sortable_table cols rows inside
          );
        Node.append_child th header;
        Node.append_child first_row th
      ) cols;
    List.iter (fun row ->
        let tr = create_html_tr document in
        Node.append_child tbody tr;
        List.iter (fun (_, _, proj) ->
            let td = create_html_td document in
            let cell = proj row in
            Node.append_child tr td;
            Node.append_child td cell
          ) cols
      ) rows

  let format_number s =
    let n = String.length s in
    let b = Buffer.create (n + n / 3) in
    let m = (n-1) mod 3 in
    for k = 0 to n-1 do
      Buffer.add_char b s.[k];
      if k mod 3 = m && k < n-1 then
        Buffer.add_char b ' '
    done;
    Buffer.contents b

  let create ?text ?class_name ?style name =
    let element = Document.create_element document name in
    (match text with
     | Some text -> Node.set_text_content element text
     | _ -> ());
    (match style with
     | Some style -> Element.set_attribute element "style" style
     | _ -> ());
    (match class_name with
     | Some class_name -> Element.set_class_name element class_name
     | _ -> ());
    element

  let record_table l =
    let table = create ~class_name:"vertical" "table" in
    List.iter (fun (name, value) ->
        let tr = create "tr" in
        let th = create ~text:name "th" in
        let td = create ~text:value "td" in
        Node.append_child table tr;
        Node.append_child tr th;
        Node.append_child tr td)
      l;
    table

end

module Graph = struct

  type id = int [@@js]

  type kind = Landmark.Graph.kind =
    | Normal [@js "normal"]
    | Root [@js "root"]
    | Counter  [@js "counter"]
    | Sampler [@js "sampler"]
  [@@js] [@@js.enum]

  type node = Landmark.Graph.node = {
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
  } [@@js] [@@js.verbatim_names]

  type graph = Landmark.Graph.graph = {nodes: node array; label: string} [@@js]

  let graph_of_string s =
    try graph_of_js (JSON.parse s) with Ojs_exn.Error _ -> error "Invalid input format."

  let string_of_graph s = JSON.stringify (graph_to_js s)

  let has_sys_time {nodes} =
    Array.exists (fun {sys_time; _} -> sys_time <> 0.0) nodes

  let has_allocated_bytes {nodes} =
    Array.exists (fun {allocated_bytes; _} -> allocated_bytes <> 0.0) nodes

  let aggregated_table graph =
    let graph = Landmark.Graph.aggregate_landmarks graph in
    let all_nodes =
      List.sort
        (fun {time = time1; _} {time = time2; _} -> compare time2 time1)
        (Landmark.Graph.nodes graph)
    in
    let text x = Document.create_text_node document x in
    let profile_with_sys_time =
      if has_sys_time graph then
        [text "Time", (fun x y -> compare x.sys_time y.sys_time),
         fun {sys_time; _} -> text (Printf.sprintf "%.0f" sys_time |> Helper.format_number)]
      else []
    in
    let profile_with_allocated_bytes =
      if has_allocated_bytes graph then
        [text "Allocated Bytes", (fun x y -> compare x.allocated_bytes y.allocated_bytes),
         fun {allocated_bytes; _} -> text (Printf.sprintf "%.0f" allocated_bytes |> Helper.format_number)]
      else []
    in
    let cols = [
      (text "Name", (fun x y -> compare x.name y.name),
       fun {name; _} -> text name);
      (text "Location", (fun x y -> compare x.location y.location),
       fun {location; _} -> text location);
      (text "Calls", (fun x y -> compare x.calls y.calls),
       fun {calls; _} -> text (string_of_int calls |> Helper.format_number));
      (text "Cycles", (fun x y -> compare x.time y.time),
       fun {time; _} -> text (Printf.sprintf "%.0f" time |> Helper.format_number));
    ] @ profile_with_sys_time @ profile_with_allocated_bytes
    in
    Helper.sortable_table cols all_nodes

end

module TreeView = struct
  open Helper

  let open_button = "[+]"
  let close_button = "[-]"

  let rec generate render expand children inside parent x =
    let li = create "li" in
    let div = create "div" in
    let content = render parent div x in
    Node.append_child div content;
    Node.append_child li div;
    let sons = children x in
    if sons <> [] then begin
      let span = create "span" ~text:open_button ~class_name:"collapseButton" in
      Element.set_class_name div "collapsible";
      Node.append_child div span;
      let expanded_state = ref [] in
      let ul = create "ul" in
      Node.append_child li ul;
      let do_expand () =
        Node.set_text_content span close_button;
        expanded_state := List.map (generate render expand children ul (Some x)) sons
      in
      if expand x then
        do_expand ();
      let onclick _ =
        if !expanded_state = [] then begin
          do_expand ()
        end else begin
          Node.set_text_content span open_button;
          List.iter (Node.remove_child ul) !expanded_state;
          expanded_state := []
        end
      in
      Element.set_onclick div onclick
    end;
    Node.append_child inside li;
    li

  let append render expand children inside root =
    let ul = create "ul" in
    Node.append_child inside ul;
    generate render expand children ul None root |> ignore

  let callgraph inside ({Graph.nodes} as graph) proj =
    let root =
      if Array.length nodes = 0 then
        error "callgraph: no root"
      else nodes.(0)
    in
    let intensity = Landmark.Graph.intensity ~proj graph in
    let color node =
      let rgb = Printf.sprintf "rgb(%d,%d,%d)" in
      let open Graph in
      match node.kind with
      | Normal -> begin
          let i = intensity node in
          (* Implements the bijection:
                 [0, 1] --> [0,1]
                              ______________
                   i   |--> \/ 1 - (i - 1)^2

             to "amplify" the intensity (it is a quarter of a circle).
          *)
          let i = i -. 1.0 in
          let i = i *. i in
          let i = sqrt (1.0 -. i) in
          rgb (int_of_float (255.0 *. i)) 0 0
        end
      | Root -> rgb 125 125 125
      | Counter -> rgb 0 125 200
      | Sampler -> rgb 0 200 125
    in
    let previous_info = ref None in
    let render (parent : Graph.node option) container ({Graph.name; time = node_time; kind; calls; distrib; allocated_bytes; sys_time; location; _} as node) =
      let node_value = proj node in
      let span = create "span" ~class_name:"content" ~text:name ~style:(Printf.sprintf "color:%s" (color node)) in
      Element.set_onmouseover span (fun () ->
          (match !previous_info with Some dispose -> dispose () | None -> ());
          let table =
            Helper.record_table
              ( ["Name", name; "Cycles", Printf.sprintf "%.0f" node_time |> Helper.format_number; "Calls", Printf.sprintf "%d" calls |> Helper.format_number ]
                @ (if location <> "" then ["Location", location] else [])
                @ (if sys_time <> 0.0 then ["Time", Printf.sprintf "%.0f" sys_time |> Helper.format_number ] else [])
                @ (if allocated_bytes <> 0.0 then ["Allocated bytes", Printf.sprintf "%.0f" allocated_bytes |> Helper.format_number ] else []))
          in
          let div = create "div" ~class_name:"fixed" in
          Node.append_child div table;
          Node.append_child container div;
          previous_info := Some (fun () -> Node.remove_child container div));
      (match parent, kind with
       | Some parent, Graph.Normal ->
         let parent_value = proj parent in
         let text =
           Printf.sprintf " (%2.2f%%) " (100.0 *. node_value /. parent_value)
         in
         let span_value = create ~text "span" in
         Node.append_child span span_value
       | _, Graph.Counter ->
         let text =
           Printf.sprintf " (%d calls) " calls
         in
         let span_time = create ~text "span" in
         Node.append_child span span_time
       | _, Graph.Sampler ->
         let text =
           Printf.sprintf " (%d values) " (Array.length distrib)
         in
         let span_time = create ~text "span" in
         Node.append_child span span_time
       | _ -> ());
      span
    in
    let reference = Landmark.Graph.shallow_ancestor graph in
    let depth = Landmark.Graph.depth graph in
    let expand node =
      let reference = reference node in
      let open Graph in
      depth node <= 1 || proj node > 0.1 *. proj reference
    in
    let children {Graph.sons; _} =
      let children = ref [] in
      List.iter
        (fun id -> children := nodes.(id) :: !children)
        sons;
      List.sort (fun node node' ->
          compare (proj node') (proj node)) !children
    in
    append render expand children inside root

end

let filename_onclick _ =
  let filename = Helper.input_of_id "filename" in
  let file = FileList.item (Html.Input.files filename) 0 in
  let filereader = FileReader.new_file_reader () in
  match file with
  | None -> error "Unable to open file."
  | Some file ->
    let onload _ =
      let result = FileReader.result filereader in
      match result with
      | None -> error "Error while reading file."
      | Some text ->
        let open Graph in
        let graph = graph_of_string text in
        let main = Helper.element_of_id "main" in
        Helper.removeAll main; print_endline "removed";
        Helper.show main;
        let tabs = Helper.create "ul" ~class_name:"tabs" in
        Node.append_child main tabs;
        let tab (present, title, fill) =
          if not present then [] else
            let div = Helper.create "div" in
            let title = Helper.create ~text:title "li" in
            Node.append_child tabs title;
            Node.append_child main div;
            fill div;
            [title, div]
        in
        let fill_graph proj div =
          TreeView.callgraph div graph proj;
        in
        let l = List.flatten (List.map tab [
            true, "Source Tree Cycles", fill_graph (fun {time; _} -> time);
            has_sys_time graph, "Source Tree Time",
            fill_graph (fun {sys_time; _} -> sys_time);
            has_allocated_bytes graph, "Source Tree Allocation",
            fill_graph (fun {allocated_bytes; _} -> allocated_bytes);
            true, "Aggregated Table", Graph.aggregated_table graph ])
        in
        Helper.tabs_logic l
    in
    FileReader.read_as_text filereader file;
    FileReader.set_onload filereader onload

let onload _ = begin
  let filename_button = Helper.element_of_id "filenameButton" in
  Element.set_onclick filename_button filename_onclick;
  Helper.hide (Helper.element_of_id "main")
end
let () = Window.set_onload window onload
