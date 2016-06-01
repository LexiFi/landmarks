let marc =
  let[@landmark] test x = x
  in test "marc", test 2

let () =
  let open Landmark in
  if profiling () then begin
    let open Landmark_graph in
    let cg = export () in
    let agg = aggregate_landmarks cg in
    let all_nodes = nodes agg in
    print_endline "\nLandmark reached:";
    all_nodes
      |> List.map (fun {name; _} -> name)
      |> List.sort compare
      |> List.iter print_endline
  end
