(** This test is a bit like test/ppx except we do not use the 'let[@landmark] ...' construction
  * which was not available in ocaml 4.02.
  *)

let rec fib n =
    (if n <= 1 then 1 else fib (n - 1) + fib (n - 2))[@landmark "fib"]

let rec even n =
  (match n with
  | 0 -> even0
  | n -> odd (n - 1))[@landmark "even"]
and odd n =
  (match n with
  | 0 -> odd0
  | n -> even (n - 1))[@landmark "odd"]
and even0 = true
and odd0 = false[@landmark "odd0"]

let prime n =
  (odd n && (let result =
              (let k = ref 3 in
              let m = int_of_float (sqrt (float n)) in
              try while !k <= m do
                  if n mod !k = 0 then
                    raise Exit;
                  incr k
                done;
                true
              with Exit -> false)[@landmark "result"]
            in result))[@landmark "prime"]

let rec next_prime n =
  (if prime n then n else next_prime (n + 1))[@landmark "next_prime"]

[@@@landmark "auto"]

let print s =
  Printf.printf "in: %s\n%!" s

let lm1 () = print "lm1"
let lm2 () = (print "lm2")[@landmark "lm2"]
let lm3 () = print "lm3"
let lm4 () = (print "lm4_alpha")[@landmark "lm4_alpha"]

[@@@landmark "auto-off"]

let noauto () = print "no-auto"

module M = struct
  let mod_lm0 () = (print "mod_lm0")[@landmark "mod_lm0"]
  [@@@landmark "auto"]
  let mod_lm1 () = print "mod_lm1"
  let mod_lm2 () = print "mod_lm2"[@landmark "mod_lm2"]
  let mod_lm3 () = print "mod_lm3"
  [@@@landmark "auto-off"]
  let mod_noauto () = print "mod_no-auto"
  [@@@landmark "auto"] (* Should be useless *)
end

include struct
  let incl_noauto1 () = print "incl_no-auto1"
  let incl_lm0 () = (print "incl_lm0")[@landmark "incl_lm0"]
  [@@@landmark "auto"]
  let incl_lm1 () = print "incl_lm1"
  let incl_lm2 () = (print "incl_lm2")[@landmark "incl_lm2"]
  let incl_lm3 () = print "incl_lm3"
  [@@@landmark "auto-off"]
  let incl_noauto2 () = print "incl_no-auto2"
end

let local_module () =
  let module M =
    struct
      [@@@landmark "auto"]
      let local1 () = print "local1"
      [@@@landmark "auto-off"]
      let local2 () =
        (print "local2")[@landmark "local2"]
      let local_noauto () =
        print "local_noauto"
    end
  in
  M.local1 ();
  M.local2 ();
  M.local_noauto ()

let () =
  let open Landmark in

  let main () =
    (let compute () =
      ignore (fib 10);
      ignore (next_prime 123)
    in

    compute ();

    let unit2 = (lm1 ();
     lm2 ();
     lm3 ();
     lm4 ();
     noauto ())[@landmark "not module"]
    in
    ignore unit2;
    (let open M in
     mod_lm0 ();
     mod_lm1 ();
     mod_lm2 ();
     mod_lm3 ();
     mod_noauto ())[@landmark "module"];

    (incl_lm0 ();
    incl_lm1 ();
    incl_lm2 ();
    incl_lm3 ();
    incl_noauto1 ();
    incl_noauto2 ())[@landmark "module"];


    local_module ())[@landmark "main"]
  in
  main ();
  if profiling () then begin
    let open Landmark.Graph in
    let cg = export () in
    let agg = aggregate_landmarks cg in
    let all_nodes = nodes agg in
    print_endline "\nLandmark reached:";
    all_nodes
      |> List.map (fun {name; _} -> name)
      |> List.sort compare
      |> List.iter print_endline
  end
