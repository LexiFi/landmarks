(* Issue #3 *)
type c = { x1: float }
let[@landmark] f ~x1 c =
  x1 <= c.x1

(* Issue #5 *)
let[@landmark] g : type t. t -> t = fun x -> x
let[@landmark "h"] h : type t. t -> t = fun x -> x
