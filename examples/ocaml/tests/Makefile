all: pattern.graph typecore.graph

pattern.graph: pattern.ml
	OCAML_LANDMARKS='output="pattern.graph",format=json' ../local/bin/ocamlc.opt -c pattern.ml

pattern.ml: generate_pattern.sh
	bash generate_pattern.sh 5000 > pattern.ml

typecore.graph:
	cd ../ocaml-* && OCAML_LANDMARKS='output="../tests/typecore.graph",format=json' ../local/bin/ocamlc.opt -g -nostdlib -I stdlib -I otherlibs/dynlink -strict-sequence -principal -absname -bin-annot -safe-string -strict-formats -I utils -I parsing -I typing -I bytecomp -I middle_end -I middle_end/base_types -I asmcomp -I driver -I toplevel -c typing/typecore.ml

typecore-opt.graph:
	cd ../ocaml-* && OCAML_LANDMARKS='output="../tests/typecore-opt.graph",format=json' ../local/bin/ocamlopt.opt -g -nostdlib -I stdlib -I otherlibs/dynlink -strict-sequence -principal -absname -bin-annot -safe-string -strict-formats -I utils -I parsing -I typing -I bytecomp -I middle_end -I middle_end/base_types -I asmcomp -I driver -I toplevel -c typing/typecore.ml

clean:
	rm -f pattern.cmo pattern.ml *.o *.cm* *.graph
