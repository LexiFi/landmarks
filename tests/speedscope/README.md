# Speedscope export example

This directory contains a minimal hand-instrumented OCaml program that
demonstrates exporting a landmarks profile to the
[Speedscope](https://www.speedscope.app) flame-graph viewer.

## What the example does

`example.ml` instruments four functions:

```
main
├── sort   (sort 500 000 integers)
└── compute
    └── fib  (compute fib(33) recursively)
```

## Build

From the repository root:

```
dune build
```

## Run and export

```
OCAML_LANDMARKS="format=speedscope,output=profile.json,time" \
  ./_build/default/tests/speedscope/example.exe
```

| `OCAML_LANDMARKS` option | Effect |
|---|---|
| `format=speedscope` | Write Speedscope JSON instead of the default text report |
| `output=profile.json` | Write the profile to this file instead of stderr |
| `time` | Collect wall-clock (`Sys.time`) seconds; without this, weights are in raw CPU cycles |

## Visualise

Open [speedscope.app](https://www.speedscope.app) and drag-and-drop the
generated `profile.json`, or load the pre-generated
[profile.json](profile.json) from this directory.

The "Time Order" view shows functions in the order they were called; the
"Left Heavy" view groups identical stacks, which is most useful for recursive
functions.
