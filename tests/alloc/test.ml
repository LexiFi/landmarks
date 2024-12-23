module L = Landmark

let name = "zero_alloc"
let zero_alloc = L.register name

let () =
  L.start_profiling
    ~profiling_options:{L.default_options with allocated_bytes = true} ();
  L.enter zero_alloc;
  L.exit zero_alloc

let check_allocated_bytes () =
  let open Landmark.Graph in

  let graph = L.export () in
  let node =
    root graph
    |> children graph
    |> List.filter (fun n -> n.name = name)
    |> List.hd
  in
  Printf.printf "Allocations:\n%d\n%d\n"
    (node.allocated_bytes)
    (node.allocated_bytes_major)

let () =
  check_allocated_bytes ()
