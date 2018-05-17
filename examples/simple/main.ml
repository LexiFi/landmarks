let[@landmark] zzz () =
  Unix.sleep 1

let[@landmark] main () =
  (for _ = 1 to 9 do
     zzz ()
   done)[@landmark "loop"];
  zzz ()

let () = main ()
