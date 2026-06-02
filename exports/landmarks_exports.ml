(*
   Exporter registration machinery

   Note that we could run this as part of module initialization (evaluation)
   but it gets tricky for users because this initialization takes place
   only if this library module is referenced directly or indirectly by
   a non-library module.

   To avoid relying on this fragile behavior, we require an explicit call the
   to the [register] function.
*)
let register () = Landmark.register_exporter "speedscope" Speedscope.exporter

let speedscope_exporter = Speedscope.exporter
