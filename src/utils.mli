(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright (C) 2000-2025 LexiFi                                    *)

external clock: unit -> (Int64.t [@unboxed]) =
  "caml_highres_clock" "caml_highres_clock_native" [@@noalloc]

val allocated_bytes: unit -> int
val allocated_bytes_major: unit -> int

module SparseArray :
sig
  type 'a t = {
    mutable keys : int array;
    mutable data : 'a array;
    mutable size : int;
  }
  val dummy : unit -> 'a t
  val make : 'a -> int -> 'a t
  val reset : 'a t -> unit
  val get : 'a t -> int -> 'a
  val swap : 'a array -> int -> int -> unit
  val values : 'a t -> 'a list
  val bubble : 'a t -> unit
  val is_full : 'a t -> bool
  val resize : 'a t -> unit
  val set : 'a t -> int -> 'a -> unit
end

module Stack :
sig
  module A :
  sig
    type (_, _) kind =
        Array : ('a, 'a array) kind
      | Float : (float, floatarray) kind
    val empty : ('a, 'arr) kind -> 'arr
    val make : ('a, 'arr) kind -> int -> 'a -> 'arr
    val length : ('a, 'arr) kind -> 'arr -> int
    val get : ('a, 'arr) kind -> 'arr -> int -> 'a
    val set : ('a, 'arr) kind -> 'arr -> int -> 'a -> unit
    val blit :
      ('a, 'arr) kind -> 'arr -> int -> 'arr -> int -> int -> unit
  end
  type ('a, 'arr) t = {
    kind : ('a, 'arr) A.kind;
    mutable data : 'arr;
    mutable size : int;
  }
  val dummy : ('a, 'b) A.kind -> ('a, 'b) t
  val make : ('a, 'b) A.kind -> 'a -> int -> ('a, 'b) t
  val size : ('a, 'b) t -> int
  val resize : ('a, 'b) t -> unit
  val push : ('a, 'b) t -> 'a -> unit
  val pop : ('a, 'b) t -> 'a
  val to_floatarray : ('a, floatarray) t -> floatarray
end

type landmark = {
  id : int;
  key : landmark_key;
  kind : Graph.kind;
  name : string;
  location : string;
  mutable last_parent : node;
  mutable last_son : node;
  mutable last_self : node;
}

and node = {
  landmark : landmark;
  id : int;
  children : node SparseArray.t;
  fathers : (node, node array) Stack.t;
  mutable calls : int;
  mutable recursive_calls : int;
  mutable timestamp : int64;
  distrib : (float, floatarray) Stack.t;
  floats : floats;
}

and floats = {
  mutable time : float;
  mutable allocated_bytes : int;
  mutable allocated_bytes_stamp : int;
  mutable allocated_bytes_major : int;
  mutable allocated_bytes_major_stamp : int;
  mutable sys_time : float;
  mutable sys_timestamp : float;
}

and landmark_key = { key : string; landmark : landmark; }

and counter = landmark

and sampler = landmark

module W: Weak.S with type data = landmark_key

val new_floats : unit -> floats

val new_node:
  landmark -> node -> bool -> (unit -> int) -> (node -> unit) -> node

type profile_output =
  | Silent
  | Temporary of string option
  | Channel of out_channel

type textual_option = {threshold : float}

type profile_format =
  | JSON
  | Textual of textual_option

type profiling_options = {
  debug : bool;
  allocated_bytes: bool;
  sys_time : bool;
  recursive : bool;
  output : profile_output;
  format : profile_format
}

val set_profiling_options : profiling_options -> unit
val profiling_options : unit -> profiling_options
val profile_with_debug : unit -> bool
val profile_with_allocated_bytes : unit -> bool
val profile_with_sys_time : unit -> bool
val profile_output : unit -> profile_output
val profile_format : unit -> profile_format
val profile_recursive : unit -> bool

val default_options: profiling_options

type profiling_state = {
  root : node;
  nodes: node_info list;
  nodes_len: int;
  current: node;
  cache_miss: int
}

and node_info = {
  node: node;
  recursive: bool;
}

val stamp_root: node -> unit
val aggregate_stat_for: node -> unit
val array_list_map: ('a -> 'b) -> 'a list -> 'b array
val export_node: node -> Graph.node

val unroll_until: node -> (node -> unit) -> node -> unit
