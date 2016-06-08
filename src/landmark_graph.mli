(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)

(** All about exporting profiling results *)

(** A {i callgraph} is a tree where each {! node} is an instance
    of a {!Landmark.landmark} representing the entering and exiting
    of instrumented code in the execution path.  *)

(** Identifies nodes *)
type id = int

(** The kind of node. *)
type kind =
  | Normal (** Usual landmarks *)
  | Root (** The special node that started with the profiling. *)
  | Counter (** Counters (see {!Landmark.counter}) *)
  | Sampler (** Samplers (set {!Landmark.sampler}) *)

(** The type exported view of a node. *)
type node = {
  id : id;  (** Unique identifier. *)
  kind : kind;
  landmark_id : int; (** The node is an instance of this landmark. *)
  name: string; (** Name of the landmark (see {! Landmark.register}). *)
  location: string; (** Location of the landmark (see {! Landmark.register}). *)
  calls: int; (** Number of time this node was entered. *)
  time: float; (** Time (in cycles) spent between enter and exit. *)
  sons: id list; (** The list of instance of landmarks that was entered while node was a opened. *)
  sys_time: float; (** Time (using Sys.time) spent between enter and exit. *)
  allocated_bytes: float; (** Gc.allocated_bytes between enter and exit. *)
  distrib: float array; (** For samplers only. The list of collected samples. *)
}

(** {3 Callgraph } *)

(** The type of callgraphs. *)
type graph = { nodes : node array }

val nodes: graph -> node list
(** Returns all nodes of a graph. *)

val root: graph -> node
(** Returns the root of a graph. *)

val sons: graph -> node -> node list
(** Returns the sons of node([sons graph node] is equivalent to
    [List.map (node_of_id graph) node.sons] *)

val label: graph -> node -> string
(** Returns a fully qualified name if it is needed. *)

val graph_of_nodes: node list -> graph
(** Build a graph from a list of nodes. *)


(** {3 Traversal } *)

val path_dfs: (bool -> node list -> node -> unit) ->
                      (node list -> node -> unit) -> graph -> unit
(** [path_dfs f g graph] traverses the graph in the depth-first fashion starting
    from the root. At each step we call [f visited path v] or [g path v] where
    [v] is the visited node and [path] is the path from the root that led us to
    that node. The function [g] is called when the visited node [v] belongs to
    [path]; it indicates a loop (and the traversal does not continue with the
    sons of g). The function [f] is called when [v] does not belong to [path].
    The flag [visited] is true when the vertex has already been visited. *)

val dfs: (node list -> node -> unit) ->
         (node list -> node -> unit) -> graph -> unit
(** A specialization of [path_dfs] that does not need to read the visited flag. *)

(** {3 Utility functions } *)

val depth: graph -> node -> int
(** Returns the depth to the root of the node (it is better to partially apply
    the function, if you need to call multiple times on the same graph). *)

val shallow_ancestor: graph -> node -> node
(** Returns the oldest ancestor of a node that is not the root (if it exists) or the root if it does not exist. *)

val intensity: ?proj:(node -> float) -> graph -> node -> float
(** Returns an arbitrary number between 0.0 and 1.0. *)

val total_number_of_calls: graph -> int
(** Compute the sum of all calls field. *)

(** {3 Simplification / Merge / Quotienting.} *)

val aggregate_landmarks: graph -> graph
(** [aggregate_landmarks g] computes the quotient by the relation "being an
    instance of the same landmark". *)

(** {3 Output } *)

val output: out_channel -> graph -> unit
(** Pretty printed output a call graph on an out_channel. *)

val output_json: out_channel -> graph -> unit
(** Output a JSON representation of a call graph on an out_channel. *)
