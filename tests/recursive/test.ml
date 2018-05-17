let[@landmark] call f =
  f ()

let rec fp n () =
  if n > 0 then call (fp (n - 1))

let () =
  begin
    let open Landmark in
    let open Landmark.Graph in
    start_profiling ~profiling_options:{default_options with recursive = true} ();
    fp 10 ();
    stop_profiling ();
    let nb_recursive_nodes = List.length (nodes (export_and_reset ())) in
    Printf.printf "recursive: %d nodes\n%!" nb_recursive_nodes;

    start_profiling  ~profiling_options:{default_options with recursive = false} ();
    fp 10 ();
    stop_profiling ();
    let nb_nodes = List.length (nodes (export_and_reset ())) in
    Printf.printf "non-recursive: %d nodes\n%!" nb_nodes;

    assert (nb_nodes <> nb_recursive_nodes);
  end



