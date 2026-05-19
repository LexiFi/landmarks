(* Example: hand-instrumented profiling with Speedscope export.

   Build:
     dune build

   Run and write a Speedscope profile:
     OCAML_LANDMARKS="format=speedscope,output=profile.json,time" \
       ./_build/default/tests/speedscope/example.exe

   Open profile.json at https://www.speedscope.app to visualise the flame
   graph.  The file tests/speedscope/profile.json is a pre-generated example
   of what the output looks like. *)

let fib_lm     = Landmark.register "fib"
let sort_lm    = Landmark.register "sort"
let compute_lm = Landmark.register "compute"
let main_lm    = Landmark.register "main"

let rec fib n =
  Landmark.wrap fib_lm
    (fun n -> if n <= 1 then 1 else fib (n - 1) + fib (n - 2))
    n

let compute () =
  Landmark.wrap compute_lm (fun () -> ignore (fib 33)) ()

let sort_descending lst =
  Landmark.wrap sort_lm (List.sort (fun a b -> compare b a)) lst

let () =
  Landmark.enter main_lm;
  ignore (sort_descending (List.init 500_000 (fun i -> i)));
  compute ();
  Landmark.exit main_lm
