
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

module type S = sig
  type 'a t
  val a : int
  val return : 'a -> 'a t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
end

let return (module M : S) (a : 'a) : 'a M.t =
  M.return a

(* This doesn't type check! Landmarks should _not_ expand to this *)
(* let return (module X : S) =
    fun y ->
        (return ((module X) : (module S))) y *)

(* On the other hand, this type checks. We expand to this. *)
let return (module X : S) = fun y -> return (module X) y

(* various iterations of the above *)
let return (module M : S) a =
  M.return a

let return
  : (module M : S) -> 'a -> 'a M.t
  = fun (module M) a ->
  M.return a

let return
  : (module M : S) -> 'a -> 'a M.t
  = fun (module M : S) y ->
  return (module M) y

(* We cannot rename modules on which we depend (like how we do
  not rename newtypes), but an eta expansion that does not rename
  could be bad in the presence of shadowing. Hence, simply forbid
  shadowing when unpacking so that this function actually works:
  the type annotation on f can refer to the module M. *)
let poly (module M : S) (f : 'a. 'a -> 'a M.t) =
  f 0, f true

let anonM ((module _) : (module S)) x = x * 2

(* classic modular explicits examples *)
let rec mapM
  : type a b. (module M : S) -> (a -> b M.t) -> a list -> b list M.t
  = fun (module M) f ls ->
  let open M in
  match ls with
  | [] -> return []
  | a :: tl ->
    let* rest = mapM (module M) f tl in
    let* b = f a in
    return (b :: rest)

let mapM' (module M : S) (f : 'a -> 'b M.t) (ls : 'a list) : 'b list M.t =
  let open M in
  let rec map = function
    | [] -> return []
    | a :: tl ->
      let* rest = map tl in
      let* b = f a in
      return (b :: rest)
  in
  map ls

(* some other modular explicits test taken from OCaml's typing-modular-explicits *)
module type Typ = sig type t end

let test_lambda a = (fun (module T : Typ) (x : T.t) -> x) (module Int) a

let apply_weird (module M : Typ) (f : (module M : Typ) -> _) (x : M.t) : M.t =
  f (module M) x

(* various tests for optional arguments with modular explicits *)
let opt1 (module X : S) ?(a : int = X.a) (f : int -> bool) : bool = f a

(* Notice that X is a modular explicit, and hence it must be unpacked,
  but following "fun", it is not type constrained. Hence, during eta-expansion,
  we must maintain the unpacking of all module arguments in case they are
  for modular explicits. *)
let opt2
  : (module X : S) -> ?_m:(module S) -> 'a -> 'a
  = fun (module X) ?(_m: (module S) option) x -> x

module O : sig
  [@@@warning "-32"]
  val f : ?m:(module S) -> int -> int
  val f' : (module S) -> ?m:(module S) -> int -> int
end = struct
  let f ?m x =
    match m with
    | Some (m : (module S)) -> let module M = (val m) in M.a + x
    | None -> x

  let f' (n : (module S)) ?(m=n) x = let module M = (val m) in M.a + x
end

let opt3 (module X : S) ?(_m : (module S) option) x = x

let opt4 : ?_m:int -> 'a -> 'a = fun ?(_m : int option) x -> x

type 'a s = A of 'a

let opt5 (A (module M) : (module S) s) ?(x : int = M.a) y = x + y

let opt6 (A (module M) : (module S) s)
  ?x:(A (module M) : ((module S) s) = (A (module M))) y = M.a + y

let opt7
  : (module S) -> ?m:(module S) -> int -> int
  = fun x ?(m = x) i ->
  let module M = (val m) in M.a + i

let opt8
  : (module X : S) -> ?m:(module S) -> int -> int X.t
  = fun (module X) ?m:((module M) = (module X)) i ->
  X.return (M.a + i)
