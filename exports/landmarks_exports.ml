(*
   Exporter registration machinery
*)

let init () =
  Landmark.register_speedscope_exporter Speedscope.exporter

let speedscope_exporter = Speedscope.exporter
