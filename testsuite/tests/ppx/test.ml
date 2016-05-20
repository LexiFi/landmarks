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

let auto1 () = print_endline "auto1"
let[@landmark] auto2 () = print_endline "auto2"
let auto3 () = print_endline "auto3"


[@@@landmark "auto-off"]

let noauto () = print_endline "no-auto"

let () =
  let open Landmark in
  let[@landmark] () =
    Printf.printf "%d\n%!" (fib 10);
    Printf.printf "%d\n%!" (next_prime 123456789);
    auto1 ();
    auto2 ();
    auto3 ();
    noauto ();
  in
  ()
