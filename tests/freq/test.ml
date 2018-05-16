let freq () =
  let r = Landmark.clock () in
  Unix.sleep 1;
  let t = Landmark.clock () in
  Int64.(to_float (sub t r)) /. 1e9


let () =
  let freq_samples = Array.make 2 0.0 in
  for k = 0 to Array.length freq_samples - 1 do
   freq_samples.(k) <- freq ();
   Printf.printf "%2d. Measured frequency: %.1f GHz\n%!" (k + 1) freq_samples.(k);
  done;

  let nb_samples = 1000000 in
  Printf.printf "Measuring the time spent between two consecutives call to clock (using %d samples)...\n%!" nb_samples;
  let gap_samples = Array.make nb_samples 0.0 in
  let timestamp = Unix.gettimeofday () in
  for k = 0 to Array.length gap_samples - 1 do
    let t = Landmark.clock () in
    let s = Landmark.clock () in
    gap_samples.(k) <- Int64.(to_float (sub s t));
  done;
  let duration = Unix.gettimeofday () -. timestamp in
  Printf.printf "This benchmark took %f ms\n%!" (1000.0 *. duration);
  Array.sort compare gap_samples;
  let min_sample = gap_samples.(0) in
  let max_sample = gap_samples.(Array.length gap_samples - 1) in
  let avg_sample = Array.fold_left (+.) 0.0 gap_samples /. (float (Array.length gap_samples)) in
  let sq x = x *. x in
  let std_dev =
    Array.fold_left
      (fun acc x -> acc +. sq (x -. avg_sample)) 0.0 gap_samples
    /. (float (Array.length gap_samples))
    |> sqrt
  in
  Printf.printf "min = %.0f\n\
                 max = %.0f\n\
                 avg_gap = %.2f\n\
                 dev_gap = %.2f\n\n%!" min_sample max_sample avg_sample std_dev;
  Printf.printf "gap_quantiles:\n%!";
  let size = Array.length gap_samples in
  List.iter (fun x ->
    let i = int_of_float (x *. float size) in
    Printf.printf "\t%f%%: %.0f\n%!" (100.0 *. x) gap_samples.(i))
    [0.00001; 0.0001; 0.01; 0.1; 0.3; 0.5; 0.7; 0.9; 0.99; 0.999; 0.9999]


