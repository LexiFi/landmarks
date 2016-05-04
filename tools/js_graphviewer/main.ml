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
end

module Graph = struct

  type id = int [@@js]

  type kind = Landmark_graph.kind =
   | Normal 
   | Root 
   | Counter 
   | Sampler
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
  } [@@js]

  type graph = Landmark_graph.graph = {nodes: node array} [@@js]

  let graph_of_string s = [%js.to: graph] (JSON.parse s)
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
         let text = Printf.sprintf "(%2.2f%%)" (100.0 *. node_time /. parent_time) in
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
  | _ -> failwith (Printf.sprintf "Element of id '%s' should be an input element." id)

let onclick _ = 
  let source_tree_element = element_of_id "sourceTree" in
  let filename = input_of_id "filename" in
  let file = FileList.item (Html.Input.files filename) 0 in
  let filereader = FileReader.new_file_reader () in
  let filereader = match filereader with Some fr -> fr | None -> failwith "filereader" in
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
        Helper.removeAll source_tree_element;
        TreeView.callgraph source_tree_element graph
    in
    FileReader.read_as_text filereader file;
    FileReader.set_onload filereader onload

let onload _ = begin
  let filename_button = element_of_id "filenameButton" in
  Element.set_onclick filename_button onclick
end

let () = Window.set_onload window onload 
