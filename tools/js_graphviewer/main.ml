(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)

open Js_core

let document = Window.document window
module Helper = struct
  let removeAll element = 
    while 
     match Node.last_child element with
     | Some child -> Node.remove_child element child; true
     | None -> false
    do () done

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
end

module Graph = struct

  type id = int [@@js]

  type kind = Landmark_graph.kind =
   | Normal [@js "normal"]
   | Root [@js "root"]
   | Counter  [@js "counter"]
   | Sampler [@js "sampler"]
   [@@js] [@@js.enum] 

  type node = Landmark_graph.node = {
    id: int;
    kind : kind;
    landmark_id : int;
    name: string;
    filename: string;
    calls: int;
    time: float;
    sons: id list;
    sys_time: float;
    gc_stat: float;
    distrib: float array;
  } [@@js] [@@js.verbatim_names]

  type graph = Landmark_graph.graph = {nodes: node array} [@@js]

  let graph_of_string s = graph_of_js (JSON.parse s)
  let string_of_graph s = JSON.stringify (graph_to_js s)

  let aggregated_table graph = 
    let graph = Landmark_graph.aggregate_landmarks graph in
    let all_nodes =
      List.sort
        (fun {time = time1; _} {time = time2; _} -> compare time2 time1)
        (Landmark_graph.nodes graph)
    in 
    let normal_nodes = 
      List.filter (fun n -> n.kind = Normal || n.kind = Root) all_nodes
    in
    let sample_nodes = 
      List.filter (fun n -> n.kind = Sampler) all_nodes
    in
    let text x = Document.create_text_node document x in
    let profile_with_sys_time = 
      if List.exists (fun {sys_time; _} -> sys_time <> 0.0) normal_nodes then
        [text "System Time", (fun x y -> compare x.sys_time y.sys_time), 
         fun {sys_time; _} -> text (Printf.sprintf "%.0f" sys_time |> Helper.format_number)]
      else []
    in
    let profile_with_gc_stat = 
      if List.exists (fun {gc_stat; _} -> gc_stat <> 0.0) normal_nodes then
        [text "Garbage Collector", (fun x y -> compare x.gc_stat y.gc_stat),
         fun {gc_stat; _} -> text (Printf.sprintf "%.0f" gc_stat |> Helper.format_number)]
      else []
    in
    let cols = [
        (text "Name", (fun x y -> compare x.name y.name), 
                      fun {name; _} -> text name); 
        (text "Filename", (fun x y -> compare x.filename y.filename), 
                         fun {filename; _} -> text filename);
        (text "Calls", (fun x y -> compare x.calls y.calls), 
                       fun {calls; _} -> text (string_of_int calls |> Helper.format_number));
        (text "Time", (fun x y -> compare x.time y.time), 
                      fun {time; _} -> text (Printf.sprintf "%.0f" time |> Helper.format_number));
      ] @ profile_with_sys_time @ profile_with_gc_stat 
    in
    Helper.sortable_table cols all_nodes

end

let create ?text ?class_name name =
  let element = Document.create_element document name in
  (match text with
    | Some text -> Node.set_text_content element text
    | _ -> ());
  (match class_name with
    | Some class_name -> Element.set_class_name element class_name
    | _ -> ());
  element

module TreeView = struct

  let open_button = "[+]"
  let close_button = "[-]"

  let rec generate render children inside parent x =
     let li = create "li" in
     let div = create "div" in
     let content = render parent x in
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
       let onclick _ = 
         if !expanded_state = [] then begin
           Node.set_text_content span close_button;
           expanded_state := List.map (generate render children ul (Some x)) sons
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

  let append render children inside root = 
     let ul = create "ul" in
     Node.append_child inside ul;
     generate render children ul None root |> ignore

  let callgraph inside {Graph.nodes} = 
    let root =
      if Array.length nodes = 0 then 
        failwith "callgraph: no root" 
      else nodes.(0) 
    in
    let render (parent : Graph.node option) {Graph.name; time = node_time; _} =
      let span = create "span" ~class_name:"conten" ~text:name in
      (match parent with
         None -> ()
       | Some {Graph.time = parent_time; _} -> 
         let text = 
           Printf.sprintf " (%2.2f%%) " (100.0 *. node_time /. parent_time)
         in
         let span_time = create ~class_name:"time" ~text "span" in
         Node.append_child span span_time);
      span
    in
    let children {Graph.sons; _} = 
      let children = ref [] in
      List.iter
        (fun id -> children := nodes.(id) :: !children)
        sons;
      List.rev !children
    in
    append render children inside root

end

let element_of_id id = 
  match Document.get_element_by_id document id with
  | Some element -> element
  | None -> failwith (Printf.sprintf "Element of id '%s' not found" id)

let input_of_id id = 
  match Html.retype (element_of_id id) with
  | `Input input -> input
  | _ ->
    failwith (Printf.sprintf "Element of id '%s' should be an input element." id)

let filename_onclick _ = 
  let source_tree_div = element_of_id "sourceTree" in
  let aggregated_table_div = element_of_id "aggregatedTable" in
  let filename = input_of_id "filename" in
  let file = FileList.item (Html.Input.files filename) 0 in
  let filereader = FileReader.new_file_reader () in
  let filereader = 
    match filereader with Some fr -> fr | None -> failwith "filereader" 
  in
  match file with 
  | None -> failwith "Unable to open file."
  | Some file ->
    let onload _ = 
      let result = FileReader.result filereader in
      match result with
      | None -> failwith "Error while reading file."
      | Some text -> 
        let open Graph in
        let graph = graph_of_string text in
        Helper.removeAll source_tree_div;
        TreeView.callgraph source_tree_div graph;
        Graph.aggregated_table graph aggregated_table_div
    in
    FileReader.read_as_text filereader file;
    FileReader.set_onload filereader onload

let onload _ = begin
  let filename_button = element_of_id "filenameButton" in
  Element.set_onclick filename_button filename_onclick;
  (* TODO: Generalize this logic to multiple tabs *)
  let source_tree_tab = element_of_id "sourceTreeTab" in
  let source_tree_div = element_of_id "sourceTree" in
  let aggregated_table_div = element_of_id "aggregatedTable" in
  let table_tab = element_of_id "tableTab" in
  Element.set_attribute aggregated_table_div "style" "display: none";
  Element.set_class_name source_tree_tab "active";
  Element.set_onclick source_tree_tab (fun () ->
    Element.set_class_name source_tree_tab "active";
    Element.set_class_name table_tab "";
    Element.set_attribute aggregated_table_div "style" "display: none";
    Element.remove_attribute source_tree_div "style";
  );
  Element.set_onclick table_tab (fun () ->
    Element.set_class_name source_tree_tab "";
    Element.set_class_name table_tab "active";
    Element.set_attribute source_tree_div "style" "display: none";
    Element.remove_attribute aggregated_table_div "style";
  );
end
let () = Window.set_onload window onload 
