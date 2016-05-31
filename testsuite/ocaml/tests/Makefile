pattern.cmo: pattern.ml
	OCAML_LANDMARKS=output=temporary,format=json ../local/bin/ocamlc.opt -c pattern.ml

pattern.ml: generate_pattern.sh
	bash generate_pattern.sh 5000 > pattern.ml

clean:
	rm -f pattern.cmo pattern.ml *.o *.cm*
