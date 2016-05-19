pattern.cmo: pattern.ml
	OCAML_LANDMARKS=tj ../local/bin/ocamlc.opt -c pattern.ml

pattern.ml: generate_pattern.sh
	bash generate_pattern.sh 5000 > pattern.ml

clean:
	rm -f pattern.cmo pattern.ml *.o *.cm*
