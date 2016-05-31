ENV=OCAML_LANDMARKS=auto
EXTRAS=

include ../../Makefile.common

test.ml: ../ppx/test.ml
	@OCAML_LANDMARKS=remove ocamlfind ppx_tools/rewriter ../../../ppx/ppx_landmarks ../ppx/test.ml > test.ml

clean::
	rm -f test.ml

	
