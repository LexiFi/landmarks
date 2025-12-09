(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright (C) 2000-2025 LexiFi                                    *)

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

  let get t id =
    let {keys; data; size} = t in
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
  module A = struct
    type (_, _) kind =
      | Array : ('a, 'a array) kind
      | Float : (float, floatarray) kind
    let empty : type a arr. (a, arr) kind -> arr = function
      | Array -> [||]
      | Float -> Float.Array.create 0
    let make : type a arr. (a, arr) kind -> int -> a -> arr = fun kind n null ->
      match kind with
      | Array -> Array.make n null
      | Float -> Float.Array.make n null
    let length : type a arr. (a, arr) kind -> arr -> int = fun kind arr ->
      match kind with
      | Array -> Array.length arr
      | Float -> Float.Array.length arr
    let get : type a arr. (a, arr) kind -> arr -> int -> a = fun kind arr n ->
      match kind with
      | Array -> Array.get arr n
      | Float -> Float.Array.get arr n
    let set : type a arr. (a, arr) kind -> arr -> int -> a -> unit = fun kind arr n ->
      match kind with
      | Array -> Array.set arr n
      | Float -> Float.Array.set arr n
    let blit : type a arr. (a, arr) kind -> arr -> int -> arr -> int -> int -> unit = fun kind src srcpos dst dstpos n ->
      match kind with
      | Array -> Array.blit src srcpos dst dstpos n
      | Float -> Float.Array.blit src srcpos dst dstpos n
  end
  type ('a, 'arr) t = {
    kind : ('a, 'arr) A.kind;
    mutable data : 'arr;
    mutable size : int
  }
  (* /!\ Dummy cannot be resized. *)
  let dummy kind = { kind; data = A.empty kind; size = 0 }
  let make kind null n = { kind; data = A.make kind (max 1 n) null; size = 0 }
  let size {size; _} = size
  let resize ({kind; size; data} as stack) =
    if size = A.length kind data then begin
      assert (size > 0);
      let new_length = (2 * (size + 1)) - 1 in
      stack.data <- A.make kind new_length (A.get kind data 0);
      A.blit kind data 0 stack.data 0 size;
    end

  let push stack x =
    resize stack;
    A.set stack.kind stack.data stack.size x;
    stack.size <- stack.size + 1

  let pop stack =
    stack.size <- stack.size - 1;
    A.get stack.kind stack.data stack.size

  let to_floatarray {data; size; _} = Float.Array.sub data 0 size
end

type landmark = {
  id: int;
  key: landmark_key;
  kind : Graph.kind;
  name: string;
  location: string;


  mutable last_parent: node;
  mutable last_son: node;
  mutable last_self: node;
}

and node = {
  landmark: landmark;

  id: int;

  children: node SparseArray.t;
  fathers: (node, node array) Stack.t;

  mutable calls: int;
  mutable recursive_calls: int;
  mutable timestamp: Int64.t;
  distrib: (float, floatarray) Stack.t;
  floats : floats;
}

and floats = {
  mutable time: float;
  mutable allocated_bytes: int;
  mutable allocated_bytes_stamp: int;
  mutable allocated_bytes_major: int;
  mutable allocated_bytes_major_stamp: int;
  mutable sys_time: float;
  mutable sys_timestamp: float;
}

and landmark_key = {
  key: string;
  landmark: landmark;
}

and counter = landmark

and sampler = landmark

module W = Weak.Make(struct
    type t = landmark_key
    let equal (x : landmark_key) (y  : landmark_key) = x.key = y.key
    let hash (x : landmark_key) = Hashtbl.hash x.key
  end)

let new_floats () = {
  time = 0.0;
  allocated_bytes = 0;
  allocated_bytes_stamp = 0;
  allocated_bytes_major = 0;
  allocated_bytes_major_stamp = 0;
  sys_time = 0.0;
  sys_timestamp = 0.0
}

let rec landmark_root = {
  kind = Graph.Root;
  id = 0;
  name = "ROOT";
  location = __FILE__;
  key = { key = ""; landmark = landmark_root};
  last_parent = dummy_node;
  last_son = dummy_node;
  last_self = dummy_node;
}

and dummy_node = {
  landmark = landmark_root;
  id = 0;
  children = SparseArray.dummy ();
  fathers = Stack.dummy Array;
  floats = new_floats ();
  calls = 0;
  recursive_calls = 0;
  distrib = Stack.dummy Float;
  timestamp = Int64.zero
}

and dummy_key = { key = ""; landmark = landmark_root}

let new_node landmark profile_with_debug node_id_ref allocated_nodes =
  if profile_with_debug then
    Printf.eprintf "[Profiling] Allocating new node for %s...\n%!" landmark.name;
  let id = !node_id_ref in
  incr node_id_ref;
  let node = {
    landmark;
    id;

    fathers = Stack.make Array dummy_node 1;
    distrib = Stack.make Float 0.0 0;
    children = SparseArray.make dummy_node 7;

    calls = 0;
    recursive_calls = 0;
    timestamp = Int64.zero;
    floats = new_floats ();
  } in
  allocated_nodes := node :: !allocated_nodes;
  node

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

let default_options = {
  debug = false;
  allocated_bytes = true;
  sys_time = false;
  recursive = false;
  output = Channel stderr;
  format = Textual {threshold = 1.0};
}

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
