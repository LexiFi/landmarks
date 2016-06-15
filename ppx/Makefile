ppx_landmarks: ppx_landmarks.cmx
	ocamlopt -I +compiler-libs ocamlcommon.cmxa -o $@ $^ 

ppx_landmarks.cmx: ppx_landmarks.ml
	ocamlopt -I +compiler-libs -c $<

clean:
	rm -f *.cm* *.o ppx_landmarks
