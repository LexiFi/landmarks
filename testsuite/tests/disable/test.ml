[@@@landmark "disable"]
let[@landmark] f x = x+x

[@@@landmark "auto"]
let main () =
  Printf.printf "%d\n%!" (f 2)

let () =
  main ();
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
      |> List.iter print_endline;
    assert (List.length all_nodes = 1) (* only root should be reached *)
  end

