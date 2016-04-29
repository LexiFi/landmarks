main.js: main.byte
	js_of_ocaml $< 

main.byte: main.ml
	ocamlfind ocamlc\
		-package js_of_ocaml\
		-package js_of_ocaml.ppx\
		-linkpkg  $< -o $@

clean:
	rm -f main.byte main.js main.cm*

