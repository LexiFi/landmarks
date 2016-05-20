ENV=OCAML_LANDMARKS=a
EXTRAS=

include ../../Makefile.common

test.ml: ../ppx/test.ml
	@OCAML_LANDMARKS=0 ocamlfind ppx_tools/rewriter ../../../ppx/landmarks_ppx.opt ../ppx/test.ml > test.ml

clean::
	rm -f test.ml

	
