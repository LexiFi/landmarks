(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)

type id = int


type kind = Normal | Root | Counter | Sampler

type node = {
  id : id;
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
}

type graph = { nodes : node array }

val graph_of_nodes: node list -> graph

val nodes: graph -> node list
(** Returns all nodes of a graph. *)

val root: graph -> node
(** Returns the root of a graph. *)

val sons: graph -> node -> node list
(** Returns the sons of node([sons graph node] is equivalent to
    [List.map (node_of_id graph) node.sons] *)

val label: graph -> node -> string
(** Returns a fully qualified name (ie. filename.name) if it is needed. *)

val output: out_channel -> graph -> unit
(** Pretty printed output a call graph on an out_channel. *)

val output_json: out_channel -> graph -> unit
(** Output a JSON representation of a call graph on an out_channel. *)

val dfs: (node list -> node -> unit) ->
         (node list -> node -> unit) -> graph -> unit

val path_dfs: (bool -> node list -> node -> unit) ->
                      (node list -> node -> unit) -> graph -> unit
(** [dfs f g graph] traverses the graph in the depth-first fashion starting
    from the root. At each step we call [f visited path v] or [g path v] where
    [v] is the visited node and [path] is the path from the root that led us to
    that node. The function [g] is called when the visited node [v] belongs to
    [path]; it indicates a loop (and the traversal does not continue with the
    sons of g). The function [f] is called when [v] does not belong to [path].
    The flag visited] is true when the vertex has already been visited. *)

val depth: graph -> node -> int

val shallow_ancestor: graph -> node -> node

val intensity: graph -> node -> float
(** Returns an arbitrary number between 0.0 and 1.0. *)

val total_number_of_calls: graph -> int

val aggregate_landmarks: graph -> graph
(** [aggregate_landmarks g] computes the quotient by the relation "being an
    instance of the same landmark". *)

