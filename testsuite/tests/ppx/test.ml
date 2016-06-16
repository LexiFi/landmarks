let[@landmark] rec fib n =
    if n <= 1 then 1 else fib (n - 1) + fib (n - 2)

let[@landmark] rec even = function
  | 0 -> even0
  | n -> odd (n - 1)
and[@landmark] odd = function
  | 0 -> odd0
  | n -> even (n - 1)
and even0 = true
and[@landmark] odd0 = false

let[@landmark] prime n =
  odd n && (let[@landmark] result =
              let k = ref 3 in
              let m = int_of_float (sqrt (float n)) in
              try while !k <= m do
                  if n mod !k = 0 then
                    raise Exit;
                  incr k
                done;
                true
              with Exit -> false
            in result)

let[@landmark] rec next_prime n =
  if prime n then n else next_prime (n + 1)

[@@@landmark "auto"]

let print s =
  Printf.printf "in: %s\n%!" s

let lm1 () = print "lm1"
let[@landmark] lm2 () = print "lm2"
let lm3 () = print "lm3"

[@@@landmark "auto-off"]

let noauto () = print "no-auto"

module M = struct
  let[@landmark] mod_lm0 () = print "mod_lm0"
  [@@@landmark "auto"]
  let mod_lm1 () = print "mod_lm1"
  let[@landmark] mod_lm2 () = print "mod_lm2"
  let mod_lm3 () = print "mod_lm3"
  [@@@landmark "auto-off"]
  let mod_noauto () = print "mod_no-auto"
  [@@@landmark "auto"] (* Should be useless *)
end

include struct
  let incl_noauto1 () = print "incl_no-auto1"
  let[@landmark] incl_lm0 () = print "incl_lm0"
  [@@@landmark "auto"]
  let incl_lm1 () = print "incl_lm1"
  let[@landmark] incl_lm2 () = print "incl_lm2"
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
      let[@landmark] local2 () =
        print "local2"
      let local_noauto () =
        print "local_noauto"
    end
  in
  M.local1 ();
  M.local2 ();
  M.local_noauto ()

let () =
  let open Landmark in

  let[@landmark] main () =
    let compute () =
      ignore (fib 10);
      ignore (next_prime 12345678)
    in

    compute ();

    let[@landmark "unit"] () = (lm1 ();
     lm2 ();
     lm3 ();
     noauto ())[@landmark "not module"]
    in

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


    local_module ()
  in
  main ();
  if profiling () then begin
    let open Landmark_graph in
    let cg = export () in
    let agg = aggregate_landmarks cg in
    let all_nodes = nodes agg in
    print_endline "\nLandmark reached:";
    all_nodes
      |> List.map (fun {name; _} -> name)
      |> List.sort compare
      |> List.iter print_endline
  end
