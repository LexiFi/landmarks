
open Module_b

(* Upon exiting each load of Module_a and Module_b, there would be an
  unrecoverable exception if the landmarks were shadowed. We further check that
  we reach each landmark. *)
let () =
  dummy_eta (print_endline "Hello, world!");
  if Landmark.profiling () then begin
    let cg = Landmark.export () in
    let agg = Landmark.Graph.aggregate_landmarks cg in
    let all_nodes = Landmark.Graph.nodes agg in

    print_endline "\nLandmark reached:";
    all_nodes
    |> List.map (fun { Landmark.Graph.name; _ } -> name)
    |> List.sort compare
    |> List.iter print_endline
  end
