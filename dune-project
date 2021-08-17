(lang dune 2.7)
(name landmarks)
(version 1.4)

(maintainers "Marc Lasson <marc.lasson@lexifi.com>")
(authors "Marc Lasson <marc.lasson@lexifi.com>")

(source (github LexiFi/landmarks))

(generate_opam_files true)

(license MIT)

(package
 (name landmarks)
 (synopsis "A simple profiling library")
 (description "
Landmarks is a simple profiling library for OCaml. It provides primitives to
measure time spent in portion of instrumented code. The instrumentation of the
code may either done by hand, automatically or semi-automatically using the
ppx pepreprocessor (see landmarks-ppx package).")
 (depends (ocaml (>= 4.08)))
 )

(package
 (name landmarks-ppx)
 (synopsis "Preprocessor instrumenting code using the landmarks library")
 (description "Automatically or semi-automatically instrument your code using
landmarks library.")
 (depends
  (ocaml (>= 4.08))
  (ppxlib (>= 0.22))
  (landmarks (= 1.4)))
)
