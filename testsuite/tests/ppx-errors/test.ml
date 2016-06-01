[@@@landmark "random string"];;

[@@@landmark "auto" "no auto"];;

let () = (print_endline "hello")[@landmark "too"][@landmark "many"][@landmark "landmarks"];;


let[@landmark] two = 2;;

let () =
   let[@landmark] _ = 3 in ();;

