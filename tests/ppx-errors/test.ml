[@@@landmark "random string"];;

module M = struct
[@@@landmark "random string"];;
end;;

[@@@landmark "auto" "no auto"];;

let () = (print_endline "hello")[@landmark "too"][@landmark "many"][@landmark "landmarks"];;

let[@landmark] () = print_endline "hello";;

let () =
   let[@landmark] _ = 2+2 in ();;

