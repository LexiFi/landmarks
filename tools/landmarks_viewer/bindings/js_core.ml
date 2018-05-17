(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)

module Kinds = struct
  type unknown [@@js]
  module Node = struct
    type element [@@js]
    type text [@@js]
    type comment [@@js]
    type processing_instruction_node [@@js]
    type document [@@js]
    type document_type [@@js]
    type document_fragment [@@js]
    type deprecated [@@js]
  end

  module Html = struct
    type body [@@js]
    type input [@@js]

    type table [@@js]
    type tbody [@@js]
    type td [@@js]
    type th [@@js]
    type thead [@@js]
    type tr [@@js]
  end
end

open Kinds

module Node : sig
  type 'a t = private Ojs.t
  val t_of_js: (Ojs.t -> 'a) -> Ojs.t -> 'a t
  val t_to_js: ('a -> Ojs.t) -> 'a t -> Ojs.t

  (** Node API *)

  val append_child: 'a t -> 'b t -> unit
  val base_URI: 'a t -> string
  val clone_node: 'a t -> 'a t
  val first_child: 'a t -> unknown t option
  val has_child_nodes: 'a t -> bool
  val last_child: 'a t -> unknown t option
  val remove_child: 'a t -> 'b t -> unit

  val set_text_content: 'a t -> string -> unit
  val get_text_content: 'a t -> string -> unit

  open Kinds.Node
  val node_type: 'a t ->
    [ `Element of element t
    | `Text of text t
    | `ProcessingInstructionNde of processing_instruction_node t
    | `Comment of comment t
    | `Document of document t
    | `DocumentType of document_type t
    | `DocumentFragment of document_fragment t
    | `Deprecated of deprecated t ]
end = struct
  include ([%js] : sig
             type untyped = private Ojs.t
             val untyped_of_js: Ojs.t -> untyped
             val untyped_to_js: untyped -> Ojs.t

             val append_child: untyped -> untyped -> unit
             val base_URI: untyped -> string
             val clone_node: untyped -> untyped
             val first_child: untyped -> untyped option
             val has_child_nodes: untyped -> bool
             val last_child: untyped -> untyped option
             val node_type: untyped -> int
             val remove_child: untyped -> untyped -> unit

             val set_text_content: untyped -> string -> unit
             val get_text_content: untyped -> string -> unit
           end)
  type 'a t = untyped
  let t_of_js _ x = untyped_of_js x
  let t_to_js _ x = untyped_to_js x

  let node_type x =
    let open Kinds.Node in
    match node_type x with
    | 1 -> `Element (x : element t)
    | 3 -> `Text (x : text t)
    | 7 -> `ProcessingInstructionNde (x : processing_instruction_node t)
    | 8 -> `Comment (x : comment t)
    | 9 -> `Document (x : document t)
    | 10 -> `DocumentType (x : document_type t)
    | 11 -> `DocumentFragment (x: document_fragment t)
    | _ -> `Deprecated (x: deprecated t)
end

module Element : sig
  type 'a t = Kinds.Node.element Node.t
  val t_of_js: (Ojs.t -> 'a) -> Ojs.t -> 'a t
  val t_to_js: ('a -> Ojs.t) -> 'a t -> Ojs.t

  val has_attribute: 'a t -> string -> bool
  val set_attribute: 'a t -> string -> string -> unit
  val get_attribute: 'a t -> string -> string
  val remove_attribute: 'a t -> string -> unit

  val set_class_name: 'a t -> string -> unit
  val get_class_name: 'a t -> string

  val set_innerHTML:  'a t -> string -> unit
  val inner_HTML: 'a t -> string

  val set_outer_HTML: 'a t -> string -> unit
  val outer_HTML: 'a t -> string

  val set_onclick: 'a t -> (unit -> unit) -> unit
  val set_onmouseover: 'a t -> (unit -> unit) -> unit

  val unsafe_cast: 'a t -> 'b t
  val tag_name: 'a t -> string
end = struct
  include ([%js] :
           sig
             type untyped = Kinds.Node.element Node.t
             val untyped_of_js: Ojs.t -> untyped
             val untyped_to_js: untyped -> Ojs.t

             val has_attribute: untyped -> string -> bool
             val set_attribute: untyped -> string -> string -> unit
             val get_attribute: untyped -> string -> string
             val remove_attribute: untyped -> string -> unit

             val set_class_name: untyped -> string -> unit
             val get_class_name: untyped -> string

             val set_innerHTML:  untyped -> string -> unit
             val inner_HTML: untyped -> string

             val set_outer_HTML: untyped -> string -> unit
             val outer_HTML: untyped -> string

             val set_onclick: untyped -> (unit -> unit) -> unit
             val set_onmouseover: untyped -> (unit -> unit) -> unit
             val tag_name: untyped -> string
           end)
  type 'a t = untyped
  let t_of_js _ x = untyped_of_js x
  let t_to_js _ x = untyped_to_js x
  let unsafe_cast x = x
end

module Document = struct
  type t = Kinds.Node.document Node.t [@@js]

  include ([%js] : sig
             val set_title: t -> string -> unit
             val title: t -> string

             val get_element_by_id: t -> string -> unknown Element.t option
             val get_elements_by_class_name: t -> string -> unknown Element.t array

             val create_element: t -> string -> unknown Element.t
             val create_text_node: t -> string -> Kinds.Node.text Node.t

             val body: t -> Kinds.Html.body Element.t
           end)

  let create_html_input document =
    (create_element document "input"
     |> Element.unsafe_cast : Kinds.Html.input Element.t)
  let create_html_table document =
    (create_element document "table"
     |> Element.unsafe_cast : Kinds.Html.table Element.t)
  let create_html_tr document =
    (create_element document "tr"
     |> Element.unsafe_cast : Kinds.Html.tr Element.t)
  let create_html_td document =
    (create_element document "td"
     |> Element.unsafe_cast : Kinds.Html.td Element.t)
  let create_html_th document =
    (create_element document "th"
     |> Element.unsafe_cast : Kinds.Html.th Element.t)
  let create_html_tbody document =
    (create_element document "tbody"
     |> Element.unsafe_cast : Kinds.Html.tbody Element.t)
  let create_html_thead document =
    (create_element document "thead"
     |> Element.unsafe_cast : Kinds.Html.thead Element.t)
end

module Window : sig
  type t = private Ojs.t

  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t

  val document: t -> Document.t

  val set_onload: t -> (unit -> unit) -> unit
end = [%js]

val window: Window.t
[@@js]

val alert: string -> unit
[@@js.global]

module Console : sig
  type t = private Ojs.t

  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t

  val log: t -> Ojs.t -> unit

  val log_string: t -> string -> unit
  [@@js.call "log"]
end = [%js]

val console: Console.t
[@@js]

module JSON : sig
  val parse: string -> Ojs.t
  [@@js.global "JSON.parse"]
  val stringify: Ojs.t -> string
  [@@js.global "JSON.stringify"]
end = [%js]

module File : sig
  type t = private Ojs.t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t
  val name: t -> string
end = [%js]

module FileList : sig
  type t = private Ojs.t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t
  val item: t -> int -> File.t option
  val length: t -> int
end = [%js]

module FileReader = struct
  type state =
    | Empty [@js 0]
    | Loading [@js 1]
    | Done [@js 2] [@@js] [@@js.enum]

  include ([%js] : sig
             type t = private Ojs.t
             val t_of_js: Ojs.t -> t
             val t_to_js: t -> Ojs.t
             val new_file_reader : unit -> t [@@js.new]
             val ready_state : t -> state
             val result: t -> string option
             val set_onload: t -> (unit -> unit) -> unit
             val read_as_text: t -> File.t -> unit
           end)
end

module Html = struct
  module Input = struct
    type t = Kinds.Html.input Element.t [@@js]
    include ([%js] : sig
               val files: t -> FileList.t
             end)
  end

  let retype x =
    match String.lowercase_ascii (Element.tag_name x) with
    | "input" -> `Input (Element.unsafe_cast x : Kinds.Html.input Element.t)
    | "table" -> `Table (Element.unsafe_cast x : Kinds.Html.table Element.t)
    | "tr" -> `Tr (Element.unsafe_cast x : Kinds.Html.tr Element.t)
    | "td" -> `Td (Element.unsafe_cast x : Kinds.Html.td Element.t)
    | "body" -> `Body (Element.unsafe_cast x : Kinds.Html.body Element.t)
    | "tbody" -> `Tbody (Element.unsafe_cast x : Kinds.Html.tbody Element.t)
    | "thead" -> `Thead (Element.unsafe_cast x : Kinds.Html.thead Element.t)
    | _ -> `Unknown
end
