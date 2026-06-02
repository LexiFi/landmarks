(*
   Public interface for the landmarks-exports library
*)
(** Additional exporters for the Landmarks profiling library *)

(** Make the exporters available to the core Landmarks module.

    Specifying which exporter to use is done via the [OCAML_LANDMARKS]
    environment variable.

    The extra exporters provided by this library are:
    - [speedscope]: produces a file that can be opened at
      {{: https://www.speedscope.app } speedscope.app};

    Sample command-line invocation:
{v
     $ OCAML_LANDMARKS=format=speedscope,output=profile.json,time myprog
v}
*)
val register : unit -> unit

(** Export to the Speedscope format *)
val speedscope_exporter : Landmark.exporter
