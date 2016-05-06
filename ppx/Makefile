landmarks_ppx.byte: landmarks_ppx.cmo
	ocamlfind ocamlc -package compiler-libs ocamlcommon.cma -o $@ $<

landmarks_ppx.cmo: landmarks_ppx.ml
	ocamlfind ocamlc -package compiler-libs -c $<

clean:
	rm -f *.cm* 
