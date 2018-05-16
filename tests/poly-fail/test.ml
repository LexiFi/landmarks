let ok =
  let test = (fun x -> x)
  in test "string", test 1;;

let ko =
  let test = (fun x -> x)[@landmark "test"]
  in test "string", test 1;;
