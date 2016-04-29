(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)

open Js

module Html = Dom_html

let document = Html.window ##. document

module Helper = struct
  let removeAll (element : #Dom.node t) = 
    while 
     match element ##. lastChild |> Opt.to_option with
     | Some child ->  let _ = element ## removeChild child in true
     | None -> false
    do () done
end


module Graph = struct

  class type node = object
    method id : int t prop
    method kind : js_string t prop
    method landmark_id : int t prop
    method name : js_string t prop
    method filename : js_string t prop
    method calls : int t prop
    method time : number t prop
    method sons : int js_array t prop
    method sys_time : number t prop
    method gc_stat : number t prop
    method distrib : number js_array t prop
  end

  class type graph = object
    method nodes : node t js_array t prop
  end

  let graph_of_string s = 
    (_JSON ## parse s : graph t)

  let string_of_graph (g : graph t) = 
    _JSON ## stringify g

end

module TreeView = struct

  let open_button = Opt.return (string "[+]")
  let close_button = Opt.return (string "[-]")

  let rec generate render children inside parent x =
     let li = Html.createLi document in
     let div = Html.createDiv document in
     let content = render parent x in
     Dom.appendChild div content;
     Dom.appendChild li div;
     let sons = children x in
     if sons <> [] then begin
       let span = Html.createSpan document in
       span ##. textContent := open_button;
       span ##. className := string "collapseButton";
       div ##. className := string "collapsible";
       Dom.appendChild div span;
       let expanded_state = ref [] in
       let ul = Html.createUl document in
       Dom.appendChild li ul;
       let onclick _ = 
         if !expanded_state = [] then begin
           span ##. textContent := close_button;
           expanded_state := List.map (generate render children ul (Some x)) sons
         end else begin
           span ##. textContent := open_button;
           List.iter (Dom.removeChild ul) !expanded_state;
           expanded_state := []
         end;
         bool true
       in
       div ##. onclick := Html.handler onclick;
     end;
     Dom.appendChild inside li;
     li

  let append render children inside root = 
     let ul = Html.createUl document in
     Dom.appendChild inside ul;
     generate render children ul None root |> ignore

  let callgraph (inside : Html.element t) (graph : Graph.graph t)= 
    let nodes = to_array (graph ##. nodes) in
    let root =
      if Array.length nodes = 0 then 
        failwith "callgraph: no root" 
      else nodes.(0) 
    in
    let render (parent : Graph.node t option) (node : Graph.node t) =
      let span = Html.createSpan document in
      span ##. className := string "content";
      span ##. textContent := Opt.return (node ##. name);
      (match parent with
         None -> ()
       | Some parent -> 
         let parent_time = parent ##. time |> float_of_number in
         let node_time = node ##. time |> float_of_number in
         let percent = Printf.sprintf "(%2.2f%%)" (100.0 *. node_time /. parent_time) in
         let span_time = Html.createSpan document in
         span_time ##. className := string "time";
         span_time ##. textContent := Opt.return (string percent);
         Dom.appendChild span span_time);
      (span :> Html.element t)
    in
    let children (node : Graph.node t)  = 
      let sons = ref [] in
      Array.iter
        (fun id -> sons := nodes.(id) :: !sons)
        (node ##. sons |> to_array);
      List.rev !sons
    in
    append render children inside root

end

let element_of_id id = Opt.get
    (document ## getElementById (string id)) 
    (fun () -> failwith (Printf.sprintf "element of id '%s' not found" id) )

let input_of_id id = 
  let element = element_of_id id in
  match Html.tagged element with
  | Html.Input i -> i
  | _ -> failwith (Printf.sprintf "element of id '%s' is not an input element" id)

let onclick _ = 
  let main_element = element_of_id "main" in
  let filename = input_of_id "filename" in
  let files = filename ##. files in
  Optdef.iter files (fun files -> 
      let file = files ## item 0 in
      Opt.iter file (fun file -> 
          let text = File.readAsText file in
          Lwt.on_success text (fun text ->
              let open Graph in
              let graph = graph_of_string text in
              Helper.removeAll main_element;
              TreeView.callgraph main_element graph))
  );
  bool true

let onload _ = begin
  let filename_button = element_of_id "filenameButton" in
  filename_button ##. onclick := Html.handler onclick;
  bool true
end

let () = Html.window ##. onload := Html.handler onload
