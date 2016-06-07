let[@landmark] hello _ =
   print_endline "hello"

let () =
  ((begin
    let a = Array.init 100 (Thread.create hello) in
    Array.iter Thread.join a;
  end)[@landmark "main"] ; ())
