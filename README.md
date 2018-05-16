Landmarks: A Simple Profiling Library
=====================================

*Landmarks* is a simple profiling library for OCaml. It provides primitives to
delimit portions of code and measure the performance of instrumented code at
runtime. The available measures are obtained by aggregating CPU cycles (using
the cpu's time stamp counter), applicative time (using `Sys.time`) and
allocated bytes (with `Gc.allocated_bytes`). The instrumentation of the code
may either done by hand, automatically or semi-automatically using a PPX
extension.

During the execution of your program, the traversal of instrumented code by
the control flow is recorded as a "callgraph" that carries the collected
measures. The results may be browsed either directly on the console, or
by exporting results to a simple web-application (that you can either build
locally or [access online](http://LexiFi.github.io/landmarks/viewer.html)).

This tool is intended to be used as a way to find where the time is spent in
your programs (and not benchmark independent pieces of code like
[Core_bench](https://github.com/janestreet/core_bench)) while providing results
that only correspond to the instrumented portion of your OCaml code (contrary to
tools that directly work with the binary executable like
[gprof](https://sourceware.org/binutils/docs/gprof/) or
[perf](https://perf.wiki.kernel.org)).

For more information, you may browse the [API](http://LexiFi.github.io/landmarks/api/).

Installation
------------

- With opam:
```
opam install landmarks
```
and 
```
opam install landmarks-viewer
```
for installing the landmarks viewer. 

- Manually:
```
git clone https://github.com/LexiFi/landmarks.git
cd landmarks
jbuilder build @install
```
and `make uninstall` to remove installed files.


Usage with dune/jbuilder
------------------------

Simply use the library `landmarks` and the preprocessor `landmarks.ppx` to
benchmark your executables and libraries. For instance, the following `jbuild`
file builds the executable `test` using the `landmarks` library and its PPX. 
```
(executable
  ((name test)
   (libraries (landmarks))
   (preprocess (pps (landmarks.ppx)))
  ))
```

Usage with ocamlfind
--------------------

* Compiling and linking:
```
  ocamlfind ocamlopt -c -package landmarks prog.ml
  ocamlfind ocamlopt -o prog -package landmarks -linkpkg prog.cmx
```
You can replace "ocamlopt" by "ocamlc" to compile the program in
bytecode.

* With the PPX extension:
```
  ocamlfind ocamlopt -c -package landmarks.ppx prog.ml
  ocamlfind ocamlopt -o prog -package landmarks -linkpkg prog.cmx
```

* Launching the viewer (when available):
```
x-www-browser "$(ocamlfind query landmarks-viewer)/landmarks_viewer.html"
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

The `register` function declares new landmarks and should be used at the
toplevel. The functions `enter` and `exit` are used to delimit the portion
of code attached to a landmark. At the end of the profiling, we retrieve for
each landmark the aggregated time information spent executing the corresponding
piece of code. During the execution, a trace of each visited landmark is also
recorded in order to build a "callgraph".

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
  `ocamlfind ocamlc -o prog -package landmarks -package unix -linkpkg prog.ml`)

The induced callgraph is:
```
- 100.00% : main
|   - 90.00% : loop
|   |   - 100.00% : sleep
|   - 10.00% : sleep

```
which can be paraphrased as:
- 100% of time is spent inside the main landmark,
- 90% of time spent inside the main landmark is spent in the loop landmark,
- 10% of time spent inside the main landmark is spent in the sleep landmark,
- 100% of the time spent in loop is spent in the sleep landmark.


The `clock()` function
----------------------

The library provides a binding to the [high-performance cycles
counter](https://en.wikipedia.org/wiki/Time_Stamp_Counter) for x86 32 and 64
bits architectures (note that you may use the `landmarks-noc.cm(x)a` archive to
provide your own implementation). It is used to measure the time spent inside
instrumented code.


The PPX extension point
-----------------------

To avoid writing boilerplate code, you may use the ppx extension distributed
with this package. It allows the programmer to instrument expressions using
annotation and to automatically instrument top-level functions.

### Annotations

The value `expr [@landmark "name"]` is expanded into
```ocaml
  Landmark.enter __generated_landmark_1;
  let r =
    try expr with e -> Landmark.exit __generated_landmark_1; raise e
  in
  Landmark.exit __generated_landmark_1;
  r
```
and the declaration
```ocaml
  let __generated_landmark_1 = Landmark.register "name"
```
is appended at the top-level.

It should be pointed out that this transformation does not preserve
tail-recursive calls (and also prevents some polymorphism generalization).
To get around these problems, it is recommended to use the other provided
extension around `let ... in` and `let rec ... in`:
```ocaml
let[@landmark] f = body
```
which is expanded in :
```ocaml
let __generated_landmark_2 = Landmark.register "f"
let f = body
let f x1 ... xn =
  Landmark.enter __generated_landmark_2;
  let r =
    try f x1 ... xn with e -> Landmark.exit __generated_landmark_2; raise e
  in
  Landmark.exit __generated_landmark_2;
  r
```
when the arity `n` of `f` is obtained by counting the shallow occurrences
of `fun ... ->` and `function ... -> ` in `body`. Please note that when using
this annotation with let-rec bindings, only entry-point calls will be recorded.
For instance, in the following piece of code
```ocaml
  let () = 
    let[@landmark] rec even n = (n = 0) || odd (n - 1)
    and[@landmark] odd n = (n = 1) || n > 0 && even (n - 1)
    in Printf.printf "'six is even' is %b\n" (even 6)
```
the landmark associated with "even" will be traversed exactly once (and not three 
times !) whereas the control flow will not pass through the landmark associated 
with "odd". 

### Automatic instrumentation

The structure annotations `[@@@landmark "auto"]` and `[@@@landmark "auto-off"]`
activate or deactivate the automatic instrumentation of top-level functions
in a module. In automatic mode, all functions declarations are implicitly
annotated. Automatic instrumentation can be enabled/disabled for all files via
option `auto` in `OCAML_LANDMARKS`, as detailed below.

The OCAML_LANDMARKS environment variable
----------------------------------------

The environment variable `OCAML_LANDMARKS` is read at two different stages:
when the ppx rewriter is executed, and when the landmarks module is loaded by
an instrumented program.
This variable is parsed as a comma-separated list of items of the form
`option=argument` or `option`, where `option` is:

* During the ppx rewriter stage (at compilation time):

    * `auto` (no arguments): turns on the automatic instrumentation by default
      (behaves as if each module starts with annotation `[@@@landmark "auto"]`).

    * `threads` (no arguments): tells the ppx extension to use the
      `Landmark_threads` module instead of the module `Landmark`.

* When loading an instrumented program (at runtime):

    * `format` with possible arguments: `textual` (default) or `json`. It controls
      the output format of the profiling which is either a console friendly
      representation or json encoding of the callgraph.

    * `output` with possible argument: `stderr` (default), `stdout`, `temporary`,
      `<file>` (where `<file>` is the path of a file). It tells where to output the
      results of the profiling. With `temporary` it will print it in a temporary
      file (the name of this file will be printed on the standard error). You may
      also use `temporary:<directory>` to specify the directory where the files
      are generated.

    * `debug` with no argument. Activates a verbose mode that outputs traces on
      stderr each time the landmarks primitives are called.

    * `time` with no argument. Also collect `Sys.time` timestamps during profiling.

    * `off` with no argument. Disable profiling.

    * `on` with no argument. Enable profiling (default; may be omitted).

    * `allocation` with no argument. Also collect `Gc.allocated_byte` data.


Browsing the JSON export using the Web Viewer
---------------------------------------------

You can either compile the web viewer on your computer or
[browse it online](http://LexiFi.github.io/landmarks/viewer.html).
You need to load the JSON files using the filepicker and then you can click
around to browse the callgraph.

Instrumenting with threads
--------------------------

The `Landmark` module is not thread-safe. If you have multiple threads,
you have to make sure that at most one thread is executing instrumented
code. For that you may use the `Landmark_threads` module (included
in the landmarks-threads.cm(x)a archive) that prevents non thread-safe
functions to execute in all threads but the one which started the
profiling.

Instrumenting with OCAMLPARAM
-----------------------------

A way to blindly instrument a project is to use ocaml's OCAMLPARAM
experimental feature, by setting the environment variable OCAMLPARAM with
```
I=$(ocamlfind query landmarks),cma=landmarks.cma,cmxa=landmarks.cmxa,ppx=$(ocamlfind query landmarks)/ppx_landmarks,_"
```

Known Issue
-----------

The annotation on expressions may temper with polymorphism (this is not
the case for the let-binding annotation). For instance, the following
piece of code will fail to compile:
```ocaml
  let test = (fun x -> x)[@landmark "test"]
  in test "string", test 1
```

About
-----

This 'Landmarks' package is licensed by LexiFi under the terms of the
MIT license.

Contact: marc.lasson@lexifi.com

[![Build Status](https://travis-ci.org/LexiFi/landmarks.svg?branch=master)](https://travis-ci.org/LexiFi/landmarks)
