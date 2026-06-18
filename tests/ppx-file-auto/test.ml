let f x = x + 1

let g ?x ~y z = f x + y + z

module T = struct
  type t = A
end

let[@attribute_on_f] f x : T.t = A

let _ =
  let[@landmark] _f = fun x y : (T.t -> _) -> function A -> x in
  let[@landmark] _f x : T.t = A in
  ()

class o = object (_ : < x: T.t; ..>)
  method x = A
  method y : T.t = A
  method f x y : T.t = if x && y then A else A
  method g x y : unit -> T.t = function () -> if x && y then A else A
end
