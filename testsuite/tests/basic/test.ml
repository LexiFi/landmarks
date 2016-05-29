let call = Landmark.register "fib"
let main = Landmark.register "main"

let rec fib n =
  Landmark.wrap call
    (fun n -> if n <= 1 then 1 else fib (n - 1) + fib (n - 2)) n

let () =
  let open Landmark in
  start_profiling
    ~profiling_options:{default_options with format = JSON} ();
  enter main;
  Printf.printf "%d\n%!" (fib 7);
  exit main
