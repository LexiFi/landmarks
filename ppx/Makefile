ppx_landmarks: ../src/landmark_misc.cmx ppx_landmarks.cmx
	ocamlfind ocamlopt -package compiler-libs ocamlcommon.cmxa -o $@ $^ 

ppx_landmarks.cmx: ppx_landmarks.ml
	ocamlfind ocamlopt -package compiler-libs -I ../src -c $<

../src/landmark_misc.cmx: ../src/landmark_misc.ml
	make -C ../src $@

clean:
	rm -f *.cm* *.o ppx_landmarks
