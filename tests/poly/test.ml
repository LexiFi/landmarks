let _ =
  let[@landmark] test1 x = x
  in test1 "marc", test1 2

let _ =
  let[@landmark] test2 (type t) (x : t) = x
  in test2 "marc", test2 2

let _ =
  let obj = object method[@landmark] test3 x = x end
  in obj # test3 "marc", obj # test3 2

type _ t =
  | A : int t
  | B : bool t

let _ =
  (* type constraint is necessary for all functions here to compile *)
  let[@landmark] f_match (type a) (x : a t) : bool =
    match x with
    | A -> true
    | B -> false
  in
  let[@landmark] f_function : type a. a t -> bool = function
    | A -> true
    | B -> false
  in
  let[@landmark] rec f_poly : type a. a t -> bool = function
    | A -> true
    | B -> f_poly A || f_poly B
  in
  f_match A, f_function B, f_poly B

type empty = private |

let _ =
  (* not reachable but should compile *)
  let[@landmark] _absurd : 'a. empty -> 'a = function _ -> .
  in
  ()

let () =
  let open Landmark in
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
