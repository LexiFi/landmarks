module L = Landmark

let () =
  if not (L.profiling ()) then begin
    let profiling_options = { L.default_options with L.output = L.Channel stderr;} in
    L.start_profiling ~profiling_options ()
  end


module LM = struct
  let test = L.register "test"

  let benchmark = L.register "benchmark"

  let iterative = L.register "iterative"
  let clever  = L.register "clever"

  let recursive = L.register "recursive"

  let profiling = L.register "profiling"
  let allocation = L.register "allocation"
  let running = L.register "running"
  let misses = L.register "misses"
  let box1 = L.register "box1"
  let box2 = L.register "box2"
  let miss = L.register "miss"
  let hit1 = L.register "hit1"
  let hit2 = L.register "hit2"

  let rec_test = L.register "rec_test"
  let rec_box1 = L.register "rec_box"
  let rec_box2 = L.register "rec_alpha"

  let counter = L.register_counter "counter"
  let sampler = L.register_sampler "sampler"

  let randoms = Array.init 100 (fun k -> L.register (Printf.sprintf "random_%d" k))
end


let f1 n =
  L.enter LM.iterative;
  let r = ref 1.0 in
  for k = 0 to n do
    r := !r *. exp (float k);
  done;
  L.exit LM.iterative;
  !r

let f2 n =
  L.enter LM.clever;
  let r = exp (float (n * (n + 1) / 2)) in
  L.exit LM.clever;
  r

let rec fr n =
  L.enter LM.recursive;
  let r =
   if n = 0 then 1.0 else fr (n - 1) *. exp (float n)
  in
  L.exit LM.recursive;
  r

let small k n x = if x > 1e-3 then
  Printf.eprintf "diff%d(%d):%g is not very small.\n%!" k n x

let test n =
  let r1, r2, r3 = f1 n, f2 n, fr n in
  let diff1 = abs_float ((r1 -. r2) /. r1) in
  let diff2 = abs_float ((r2 -. r3) /. r2) in
  let diff3 = abs_float ((r1 -. r3) /. r3) in
  small 1 n diff1;
  small 2 n diff2;
  small 3 n diff3

let () = begin
  let repeat = 10 in
  let nmax = 100 in

  Printf.printf "Testing ... %!";
  L.enter LM.test;
  for n = 0 to nmax do test n done;
  L.exit LM.test;
  Printf.printf "done.\n%!";
  Printf.printf "Benchmark ... %!";
  L.enter LM.benchmark;
  for _ = 1 to repeat do
    for n = 0 to nmax do ignore (f1 n) done;
    for n = 0 to nmax do ignore (f2 n) done;
    for n = 0 to nmax do ignore (fr n) done;
  done;
  L.exit LM.benchmark;
  Printf.printf "done.\n%!";
end

let () = begin
  let open LM in
  let times = 2 in
  let topology () =
    L.increment ~times counter;
    L.sample LM.sampler (Random.float 1.0);
    L.enter box1;
      L.enter hit1;
      L.exit hit1;
      L.enter miss;
      L.exit miss;
      L.enter hit1;
      L.exit hit1;
      L.enter miss;
      L.exit miss;
    L.exit box1;
    L.enter box2;
      L.enter hit2;
      L.exit hit2;
      L.enter miss;
      L.exit miss;
      L.enter hit2;
      L.exit hit2;
      L.enter miss;
      L.exit miss;
    L.exit box2;
  in
  L.enter profiling;
    L.enter allocation;
      topology ();
    L.exit allocation;
    L.enter running;
      for _ = 1 to 10000 do topology () done;
    L.exit running;
  L.exit profiling;
end

let () = begin
  let open LM in
    L.enter rec_test;
     L.enter rec_box1;
       L.enter rec_box2;
         L.enter rec_test;
         L.exit rec_test;
       L.exit rec_box2;
     L.exit rec_box1;
    L.exit rec_test;
  end

let random_walk max_depth max_sons length =
  let rec walk depth length =
    if length > 0 && depth < max_depth then begin
      let me = Random.int (Array.length LM.randoms) in
      L.enter LM.randoms.(me);
      let sons = Random.int max_sons in
      let length_ref = ref (length - 1) in
      for _ = 1 to sons do
       length_ref := walk (depth + 1) !length_ref;
      done;
      L.exit LM.randoms.(me);
      !length_ref
    end else length
  in
  walk 0 length

let () = begin
    Printf.printf "Start a random walk ... \n%!";
    let res = random_walk 5 20 100000 in
    Printf.printf "done (%d nodes).\n%!" res
  end

open Landmark_graph

let check_invariants graph =
  let root = root graph in
  let roots = List.find_all (fun node -> node.id = root.id) (nodes graph) in
  assert (roots = [root]); (* only one root *)
  List.iter (fun node ->
      match Landmark_misc.duplicated_elements ~proj:(fun node -> node.landmark_id) (sons graph node) with
      | _ :: _ -> assert false
      | [] -> ()
    ) (nodes graph) (* at most one son with a given landmark_id *)

let check_invariants_non_aggregated graph =
  (* monotonicity & non-recursivity *)
  path_dfs (fun visited ancestor node ->
      assert (not visited);
      match ancestor with
      | [] -> assert (node.id = (root graph).id)
      | father :: _ when not visited -> assert (node.time <= father.time)
      | ancestors -> assert (List.memq node ancestors)) (fun _ _ -> assert false) graph

let reachable_landmarks graph =
  let result = ref [] in
  dfs (fun _ node -> result := node.landmark_id :: !result) (fun _ _ -> ()) graph;
  List.sort_uniq Pervasives.compare !result

let checks () =
  let graph = L.export () in
  let total = total_number_of_calls graph in

  let result = ref 0 in
  dfs (fun _ {calls; _} ->
         result := !result + calls) (fun _ _ -> ()) graph;
  if !result <> total then
    Printf.printf "result = %d, total = %d\n%!" !result total;
  assert (!result = total);

  Printf.printf "Check invariants ...\n%!";
  check_invariants graph;
  Printf.printf "Check invariants for non-aggregated graph ...\n%!";
  check_invariants_non_aggregated graph;
  Printf.printf "Aggregate graph ...\n%!";
  let aggregated_graph = aggregate_landmarks graph in
  Printf.printf "Check invariants for aggregated graph ...\n%!";
  check_invariants aggregated_graph;
  Printf.printf "Check total number of calls ...\n%!";
  let aggregated_total = total_number_of_calls aggregated_graph in
  if total <> aggregated_total then begin
     Printf.printf "Bug in aggregation primitive (%d calls before, %d after).\n%!" total aggregated_total;
     assert false
  end;
  Printf.printf "Check reachability invariant ...\n%!";
  assert (reachable_landmarks graph = reachable_landmarks aggregated_graph);
  List.iter (fun node -> if node.kind = Sampler then assert(node.calls = Array.length node.distrib)) (nodes graph)

let () = checks ()
