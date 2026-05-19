(** Export to the Speedscope format

    See https://www.speedscope.app for using the visualization app
    and https://github.com/jlfwong/speedscope/blob/main/src/lib/file-format-spec.ts
    for the annotated format specification.
*)

val export_to_channel : out_channel -> Graph.graph -> unit
(** Write a Speedscope sampled profile to [out_channel].

    If [sys_time] was collected during profiling, weights are in seconds;
    otherwise raw CPU-cycle counts are used with unit "none".

    The resulting JSON can be opened at
    {{: https://www.speedscope.app } speedscope.app}. *)
