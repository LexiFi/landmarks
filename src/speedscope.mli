val export_to_channel : out_channel -> Graph.graph -> unit
(** Write a Speedscope sampled profile to [out_channel].

    If [sys_time] was collected during profiling, weights are in seconds;
    otherwise raw CPU-cycle counts are used with unit "none".

    The resulting JSON can be opened at
    {{: https://www.speedscope.app } speedscope.app}. *)
