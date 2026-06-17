[@@@landmark "random string"];;

module M = struct
  [@@@landmark "random string"];;
end;;

[@@@landmark "auto" "no auto"];;

let () = (print_endline "hello")[@landmark "too"][@landmark "many"][@landmark "landmarks"];;

let[@landmark] () = print_endline "hello";;

let () =
  let[@landmark] _ = 2+2 in ();;

(* Errors for unhandled code *)

(* setup *)
module type S = sig
  type 'a t
  val a : int
  val return : 'a -> 'a t
end;;

(* Since module names are kept during unpacking, shadowing is an error. *)
let[@landmark] shadow ((module M) : (module S)) ((module M) : (module S)) =
  M.a;;

(* This code is only valid after 5.5. The error is on the second module M. *)
let[@landmark] shadow_poly (module M : S)
    (f : 'a. 'a -> 'a M.t) (module M : S) =
  f 0, M.return (f true);;

let[@landmark] poly_opt ?(id : 'a.'a -> 'a = Fun.id) x = id x
