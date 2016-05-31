EXTRAS=-ppx $(BASEDIR)/ppx/ppx_landmarks
ENV=OCAML_LANDMARKS="output=stderr"
include ../../Makefile.common

test.ml: ../ppx/test.ml
	@OCAML_LANDMARKS=remove ocamlfind ppx_tools/rewriter ../../../ppx/ppx_landmarks ../ppx/test.ml > /tmp/test.ml
	@for k in `seq 3`; do \
	  OCAML_LANDMARKS=auto ocamlfind ppx_tools/rewriter ../../../ppx/ppx_landmarks /tmp/test.ml > /tmp/test2.ml; \
  	  OCAML_LANDMARKS=auto ocamlfind ppx_tools/rewriter ../../../ppx/ppx_landmarks /tmp/test2.ml > /tmp/test.ml; \
	done
	@cp /tmp/test.ml test.ml


clean::
	rm -f test.ml

	
