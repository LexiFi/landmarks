OCAMLFLAGS=-w +A-4-30-42-41-48-40 -safe-string -strict-sequence -bin-annot

ppx_landmarks: ppx_landmarks.cmx
	ocamlopt $(OCAMLFLAGS) -I +compiler-libs ocamlcommon.cmxa -o $@ $^ 

ppx_landmarks.cmx: ppx_landmarks.ml
	ocamlopt $(OCAMLFLAGS) -I +compiler-libs -c $<

clean:
	rm -f *.cm* *.o ppx_landmarks
