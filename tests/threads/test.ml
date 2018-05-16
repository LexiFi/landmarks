let[@landmark] sleep _ =
   Unix.sleepf 0.1

let[@landmark] hello () =
   print_endline "Starting 100 threads ..."

let () =
  ((begin
    hello ();
    let a = Array.init 100 (Thread.create sleep) in
    Array.iter Thread.join a;
  end)[@landmark "main"]);
  let module L = Landmark_threads in
  let open L.Graph in
  let cg = L.export () in
  let agg = aggregate_landmarks cg in
  let all_nodes = nodes agg in
  print_endline "\nLandmark reached:";
  all_nodes
    |> List.map (fun {name; _} -> name)
    |> List.sort compare
    |> List.iter print_endline

