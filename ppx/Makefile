landmarks_ppx.opt: landmarks_ppx.cmx
	ocamlfind ocamlopt -package compiler-libs ocamlcommon.cmxa -o $@ $<

landmarks_ppx.cmx: landmarks_ppx.ml
	ocamlfind ocamlopt -package compiler-libs -c $<

clean:
	rm -f *.cm* landmarks_ppx.opt
