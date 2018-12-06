(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)

module Graph = Graph

(** The main module *)

external clock: unit -> Int64.t = "caml_highres_clock"
(** This function is used by the landmark infrastructure to
    measure the number of cycles inside landmarks. *)

exception LandmarkFailure of string

(** {3 Landmarks} *)

(** {i Landmarks} identify portions of code, they are registered
with the function {! register} and delimited by {! enter} and {! exit}. *)

(** The type of landmarks. *)
type landmark

val register: ?location:string -> string -> landmark
(** [register name] registers a new landmark.
    Should always be called at top-level. *)

val enter: landmark -> unit
(** Begins a landmark block.
    /!\ Landmark blocks should be well-nested, otherwise a failure will be
        raised during profiling. *)

val exit: landmark -> unit
(** Ends a landmark block. *)

val wrap: landmark -> ('a -> 'b) -> 'a -> 'b
(** Puts landmark blocks around a function (and close the block and re-raise
    in case of uncaught exception). *)

val unsafe_wrap: landmark -> ('a -> 'b) -> 'a -> 'b
(** Puts landmark blocks around a function without catching exceptions. *)

(** {3 Counter and samplers} *)

(** {i Counters} are similar to landmarks except they represent
    empty pieces of code. Their only primitive is {!increment} which
    adds a constant to the field [calls]. {i Samplers} are used to
    collect floats. *)

(** The type of counters. *)
type counter

val register_counter: string -> counter
(** [register_counter name] registers a new counter.
    Should always be called at top-level. *)

val increment: ?times:int -> counter -> unit
(** Increments the number of calls attached to the counter. *)

(** The type of samplers. *)
type sampler

val register_sampler: string -> sampler
(** [register_counter name] registers a new sampler. *)

val sample: sampler -> float -> unit
(** Collects a float. *)

(** {3 Manage profiling } *)

val profiling: unit -> bool
(** Checks if the profiling is ongoing. *)

(** Where to output results. *)
type profile_output =
  | Silent (** Disables the automatic output of profiling results
    when the program ends. *)
  | Temporary of string option (** Writes the results in a temporary files
                                   and prints its path on stderr. *)
  | Channel of out_channel (** Writes in the results in out_channel. *)


type textual_option = {threshold : float}

(** The output format for the results.*)
type profile_format =
  | JSON (** Easily parsable export format. *)
  | Textual of textual_option (** Console friendly output; nodes below the
                                  threshold (0.0 <= threshold <= 100.0) are
                                  not displayed in the callgraph. *)

(** The profiling options control the behavior of the landmark infrastructure. *)
type profiling_options = {
  debug : bool;
    (** Activates a verbose mode that outputs traces on stderr each time
        the landmarks primitives are called. Default: false. *)

  allocated_bytes: bool;
    (** Also collect {! Gc.allocated_bytes} during profiling. *)

  sys_time : bool;
    (** Also collect {! Sys.time} timestamps during profiling. *)

  recursive : bool;
    (** When false, the recursive instances of landmarks (entering
        a landmark that has already been entered) are ignored (the number of
        calls is updated but it does not generate a new node in the callgraph).*)

  output : profile_output;
    (** Specify where to output the results. *)

  format : profile_format
    (** Specify the output format. *)
}

val default_options: profiling_options
(** The default {!profiling_options}. *)

val set_profiling_options: profiling_options -> unit
(** Sets the options. *)

val profiling_options: unit -> profiling_options
(** Get the options. *)

val start_profiling: ?profiling_options:profiling_options -> unit -> unit
(** Starts the profiling. *)

val stop_profiling: unit -> unit
(** Stops the profiling. *)

val reset: unit -> unit
(** Reset the profiling information gathered by the current process. *)

val export: ?label:string -> unit -> Graph.graph
(** Export the profiling information of the current process. *)

val export_and_reset: ?label:string -> unit -> Graph.graph
(** Export the profiling information of the current process; then reset
    internal state. *)

val merge: Graph.graph -> unit
(** Aggregate the profiling information (exported by another process) to the
    current one. This should is used by the master process to merge exported
    profiles of workers. *)

val push_profiling_state: unit -> unit
(** Save the state of the profiler on a stack to be retrieved later by [pop_profiling_state ()]. *)

val pop_profiling_state: unit -> unit
(** See [push_profiling_state ()]. *)