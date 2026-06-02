(* Example: profiling with Speedscope export using PPX instrumentation.

   Build:
     dune build

   Run and write a Speedscope profile:
     OCAML_LANDMARKS="format=speedscope,output=profile.json,time" \
       ./_build/default/tests/speedscope/example.exe

   Open profile.json at https://www.speedscope.app to visualise the
   flame graph.  A pre-generated example is in tests/speedscope/profile.json.

   The call tree below grows and shrinks to form a visible flame shape:

     main
     ├── prepare          (sort a list)
     │   └── make_input   (build the list)
     ├── run
     │   ├── phase_a      (fib — deep recursion)
     │   └── phase_b      (fold over a list)
     └── summarise        (cheap post-processing)
*)

let[@landmark] rec fib n =
  if n <= 1 then 1 else fib (n - 1) + fib (n - 2)

let[@landmark] make_input n =
  List.init n (fun i -> n - i)

let[@landmark] prepare n =
  List.sort compare (make_input n)

let[@landmark] phase_a () = ignore (fib 33)

let[@landmark] phase_b lst =
  List.fold_left (fun acc x -> acc + x) 0 lst

let[@landmark] run lst =
  phase_a ();
  ignore (phase_b lst)

let[@landmark] summarise lst =
  List.length lst

let[@landmark] main () =
  let lst = prepare 300_000 in
  run lst;
  ignore (summarise lst)

let () =
  Landmarks_exports.register ();
  main ()
