EXTRAS=-ppx $(BASEDIR)/ppx/landmarks_ppx.opt
ENV=OCAML_LANDMARKS=e
include ../../Makefile.common

test.ml: ../ppx/test.ml
	OCAML_LANDMARKS=0 ocamlfind ppx_tools/rewriter ../../../ppx/landmarks_ppx.opt ../ppx/test.ml > /tmp/test.ml
	for k in `seq 2`; do \
	  OCAML_LANDMARKS=a ocamlfind ppx_tools/rewriter ../../../ppx/landmarks_ppx.opt /tmp/test.ml > /tmp/test2.ml; \
  	  OCAML_LANDMARKS=a ocamlfind ppx_tools/rewriter ../../../ppx/landmarks_ppx.opt /tmp/test2.ml > /tmp/test.ml; \
	done
	cp /tmp/test.ml test.ml


clean::
	rm -f test.ml

	
