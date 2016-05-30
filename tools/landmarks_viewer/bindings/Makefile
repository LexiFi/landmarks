.PHONY: all clean

js_core.cmo: js_core.ml
	ocamlfind ocamlc -c -package gen_js_api.ppx js_core.ml

clean:
	rm -f *.cm*
