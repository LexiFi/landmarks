let freq () = 
  let r = Landmark.clock () in
  Unix.sleep 1;
  let t = Landmark.clock () in
  Int64.(to_float (sub t r)) /. 1e9


let () = 
  let size = 3 in 
  let result = Array.make size 0.0 in
  for k = 0 to size - 1 do 
   result.(k) <- freq ();
   Printf.printf "%2d. Measured frequency: %.1f GHz\n%!" (k + 1) result.(k);
  done
