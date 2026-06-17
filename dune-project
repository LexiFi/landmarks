(lang dune 3.16)
(name landmarks)
(version 1.7)

(maintainers "Marc Lasson <marc.lasson@lexifi.com>")
(authors "Marc Lasson <marc.lasson@lexifi.com>")

(source (github LexiFi/landmarks))

(generate_opam_files true)

(license MIT)

(package
 (name landmarks)
 (synopsis "A simple profiling library")
 (description
    "Landmarks is a simple profiling library for OCaml. It provides \
     primitives to measure time spent in portion of instrumented code. \
     The instrumentation of the code may either done by hand, automatically \
     or semi-automatically using the ppx preprocessor (see landmarks-ppx \
     package)."
 )
 (depends
   (ocaml (>= 4.08))
   (js_of_ocaml (and :with-test (> 5)))
 )
 (conflicts ocaml-option-bytecode-only)
)

(package
 (name landmarks-ppx)
 (synopsis "Preprocessor instrumenting code using the landmarks library")
 (description "Automatically or semi-automatically instrument your code using
landmarks library.")
 (depends
  (ocaml (>= 4.08))
  (ppxlib (>= 0.36))
  (landmarks (= 1.7))
 )
)

(package
 (name landmarks-speedscope)
 (synopsis "Additional export formats for the Landmarks profiling library")
 (description
   "This provides export to the Speedscope format and possibly more formats \
    in the future.")
 (depends
  (landmarks (= :version))
  (yojson (>= 3.0.0))
 )
)
