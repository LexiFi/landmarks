let _ =
  let[@landmark] test1 x = x
  in test1 "marc", test1 2

let _ =
  let[@landmark] test2 (type t) (x : t) = x
  in test2 "marc", test2 2

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
