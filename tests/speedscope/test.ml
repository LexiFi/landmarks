open Landmark.Graph

(* Build a fixed graph with known values so the Speedscope output is
   deterministic and can be compared against test.out.expected. *)

let make_node id kind name location calls time children =
  { id; kind; name; landmark_id = name; location; calls; time;
    children; sys_time = 0.0; allocated_bytes = 0;
    allocated_bytes_major = 0; distrib = Float.Array.create 0 }

let () =
  (* Graph:
       root (Root)
       ├── foo  time=0.50
       └── bar  time=0.25
     unit = "none" (no sys_time)
  *)
  let root = make_node 0 Root "ROOT" "" 1 0.75 [1; 2] in
  let foo  = make_node 1 Normal "foo" "test.ml:10" 5 0.50 [] in
  let bar  = make_node 2 Normal "bar" "test.ml:20" 3 0.25 [] in
  let graph = graph_of_nodes ~label:"test" [root; foo; bar] in
  Landmarks_exports.speedscope_exporter stdout graph
