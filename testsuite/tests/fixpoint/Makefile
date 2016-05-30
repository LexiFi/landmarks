EXTRAS=-ppx $(BASEDIR)/ppx/ppx_landmarks
ENV=OCAML_LANDMARKS=e
include ../../Makefile.common

test.ml: ../ppx/test.ml
	@OCAML_LANDMARKS=0 ocamlfind ppx_tools/rewriter ../../../ppx/ppx_landmarks ../ppx/test.ml > /tmp/test.ml
	@for k in `seq 3`; do \
	  OCAML_LANDMARKS=a ocamlfind ppx_tools/rewriter ../../../ppx/ppx_landmarks /tmp/test.ml > /tmp/test2.ml; \
  	  OCAML_LANDMARKS=a ocamlfind ppx_tools/rewriter ../../../ppx/ppx_landmarks /tmp/test2.ml > /tmp/test.ml; \
	done
	@cp /tmp/test.ml test.ml


clean::
	rm -f test.ml

	
