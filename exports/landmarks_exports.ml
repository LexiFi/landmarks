(*
   Exporter registration machinery
*)

let init () =
  Landmark.register_speedscope_exporter Speedscope.exporter;
  Landmark.init ()

let speedscope_exporter = Speedscope.exporter
