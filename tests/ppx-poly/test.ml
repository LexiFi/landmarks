
(*
  This executable should have no output. All functions compile.
*)

(* type of _none cannot be inferred *)
let f_none (_none : 'a. 'a option) : unit =
  ()

let f_none' : ('a. 'a option) -> unit = fun _none ->
  ()

let _ = object
  method f_method (_none : 'a. 'a option) : unit = ()
end

(* Because newtypes are in the eta-expansion, do some newtype tests *)
let f_a (x : int) (type a) (y : a) = x, y

(* mix polymorphic annotations with newtypes *)
let f_ab (x : int) (type a) (y : a) (type b) (z : 'a. 'a -> a * b) = x, y, z

(* ensure optional args still work *)
let f_op a ?(b : 'a = a) a =
  a, b

(* mix optional arguments and forall types *)
let f_op
  : 'a -> ('b. 'b) -> ?b:'a -> ('b. 'b) -> 'a * 'a
  = fun a _ ?(b : 'a = a) a ->
  a, b

