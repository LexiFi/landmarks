(* Issue #3 *)
type c = { x1: float}
let[@landmark] f ~x1 c =
  x1 <= c.x1
