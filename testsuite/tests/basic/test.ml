

let call = Landmark.register "fib"
let main = Landmark.register "main"

let rec fib n =
  Landmark.wrap call 
    (fun n -> if n <= 1 then 1 else fib (n - 1) + fib (n - 2)) n


let () = 
  Landmark.start_profiling ();
  Landmark.enter main;
  Printf.printf "%d\n%!" (fib 10);
  Landmark.exit main
