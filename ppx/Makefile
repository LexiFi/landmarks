ppx_landmarks: ppx_landmarks.cmx
	ocamlfind ocamlopt -package compiler-libs ocamlcommon.cmxa -o $@ $<

ppx_landmarks.cmx: ppx_landmarks.ml
	ocamlfind ocamlopt -package compiler-libs -c $<

clean:
	rm -f *.cm* *.o ppx_landmarks
