EXTRAS=-dsource -ppx $(BASEDIR)/ppx/landmarks_ppx.opt
ENV=OCAML_LANDMARKS=auto
include ../../Makefile.common

test.ml: ../ppx/test.ml
	OCAML_LANDMARKS=0 ocamlfind ppx_tools/rewriter ../../../ppx/landmarks_ppx.opt ../ppx/test.ml > test.ml

clean::
	rm -f test.ml

	
