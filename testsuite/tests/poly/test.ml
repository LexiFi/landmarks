let marc =
  let[@landmark] test = (); fun x -> x
  in test "marc", test 2
