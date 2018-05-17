Landmarks: Examples
===================

These directories contain simple programs that can be built and 
test in the following two ways: 

1. jbuilder: 
```
  jbuilder build ./main.exe
  OCAML_LANDMARKS=on jbuilder exec ./main.exe
```
2. ocamlfind + Makefile: 
```
  make
  OCAML_LANDMARKS=on ./main.exe
```
