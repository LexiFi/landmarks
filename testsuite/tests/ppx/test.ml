let[@landmark] rec fib n =
    if n <= 1 then 1 else fib (n - 1) + fib (n - 2)

let[@landmark] rec even = function
  | 0 -> even0
  | n -> odd (n - 1)
and[@landmark] odd = function
  | 0 -> odd0
  | n -> even (n - 1)
and[@landmark] even0 = true
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

let lm1 () = print_endline "lm1"
let[@landmark] lm2 () = print_endline "lm2"
let lm3 () = print_endline "lm3"

[@@@landmark "auto-off"]

let noauto () = print_endline "no-auto"

module M = struct
  let[@landmark] mod_lm0 () = print_endline "mod_lm0"
  [@@@landmark "auto"]
  let mod_lm1 () = print_endline "mod_lm1"
  let[@landmark] mod_lm2 () = print_endline "mod_lm2"
  let mod_lm3 () = print_endline "mod_lm3"
  [@@@landmark "auto-off"]
  let mod_noauto () = print_endline "mod_no-auto"
end


let () =
  let open Landmark in
  let[@landmark] () =
    Printf.printf "%d\n%!" (fib 10);
    Printf.printf "%d\n%!" (next_prime 123456789);

    (lm1 ();
     lm2 ();
     lm3 ();
     noauto ())[@landmark "not module"];

    (let open M in
    mod_lm0 ();
    mod_lm1 ();
    mod_lm2 ();
    mod_lm3 ();
    mod_noauto ())[@landmark "module"]
  in
  ()
