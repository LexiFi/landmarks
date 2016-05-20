let[@landmark] id x = (); x

let () = print_endline (id "hello"); print_endline ((id id) "hello")

