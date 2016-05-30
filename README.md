Landmarks: A Simple Profiling Library
=====================================

*Landmarks* is a simple profiling library for OCaml. It provides primitives to
measure time spent in portion of instrumented code. The instrumentation of the
code may either done by hand, automatically or semi-automatically using a PPX
extension. The result of the benchmark may be browsed either directly on the
console, by exporting results to a simple web-application (that you can either
build locally or access online). 

Installation
------------

- Requirements:
   * findlib (aka ocamlfind)

- Optional requirements:
   * gen_js_api

- With opam (*not yet available*):
```
opam install landmarks
```

- With opam (development version):
```
opam pin add landmarks https://github.com/mlasson/landmarks.git
```

- Manually:
```
git clone https://github.com/mlasson/landmarks.git
cd landmarks
make
make install
```
and `make uninstall` to remove installed files.

Usage
-----

* Compiling and linking:
```
  ocamlfind ocamlopt -c -package landmarks prog.ml
  ocamlfind ocamlopt -o prog -package landmarks landmarks.cmxa prog.cmx
```
You can replace "ocamlopt" by "ocamlc" to compile the program in 
bytecode.

* With the PPX extension:
```
  ocamlfind ocamlopt -c -package landmarks.ppx prog.ml
  ocamlfind ocamlopt -o prog -package landmarks.ppx landmarks.cmxa prog.cmx
```

* Launching the viewer (when available):
```
x-www-browser $(ocamlfind query landmarks)/landmarks_viewer.html
```
You may want to replace "x-www-browser" with your system's way to 
invoke your favorite web-browser from the command line. It has
to support javascript. 


Benchmarking manually
---------------------

There are three main primitives:
```ocaml
  val register: string -> landmark
  val enter: landmark -> unit
  val exit: landmark -> unit
```

The `register` function declares new landmarks and should be used at the toplevel and
the functions `enter` and `exit` are used to delimit the portion of code attached to 
a landmark. At the end of the profiling, we retrieve for each landmark the 
aggregated time information spent executing the corresponding piece of code. During
the execution, a trace of each visited landmark is also recorded which allows to 
build a "callgraph".

For example:
```ocaml
open Landmark

let loop = register "loop"
let sleep = register "sleep"
let main = register "main"

let zzz () =
  enter sleep;
    Unix.sleep 1;
  exit sleep

let () =
  begin
    start_profiling ();
    enter main;
      enter loop;
        for _ = 1 to 9 do
          zzz ()
        done;
      exit loop;
      zzz ();
    exit main;
  end
```
(This file can be compiled with 
  `ocamlfind ocamlc -o prog -package landmarks landmarks.cma unix.cma prog.ml`)

The induced callgraph is:
```
- 100.00% : main
|   - 90.00% : loop
|   |   - 100.00% : sleep
|   - 10.00% : sleep

```
Which can be paraphrased as: 
- 100% of time is spent inside the main landmark,
- 90% of time spent inside the main landmark is spent in the loop landmark, 
- 10% of time spent inside the main landmark is spent in the sleep landmark, 
- 100% of the time spent in loop is spent in the sleep landmark. 

Troubleshooting
---------------

1. The best way to blindly instrument a project is to use ocaml's OCAMLPARAM
experimental feature, by setting the environment variable OCAMLPARAM with 
"I=$LM_DIR,cma=landmarks.cma,cmxa=landmarks.cmxa,ppx=landmarks_ppx,_". However, 
the current implementation of OCAMLPARAM does not allow to benchmark easily
projects that build archives, shared libraries and packages. This 
pull-request[https://github.com/ocaml/ocaml/pull/591] propose some improvments
of OCAMLPARAM to circumvent these problems.

2. Polymorphism ...


About
-----

This 'Landmarks' package is licensed by LexiFi under the terms of the
MIT license.

Contact: marc.lasson@lexifi.com
