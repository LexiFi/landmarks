BASEDIR=../..

main.js: main.byte
	js_of_ocaml --pretty --disable genprim +gen_js_api/ojs_runtime.js extra_runtime.js $< 

main.byte: main.ml bindings
	ocamlfind ocamlc -o $@ -no-check-prims\
		-I $(BASEDIR)/src/            \
		-I bindings                   \
		landmarks-noc.cma             \
		js_core.cmo                   \
		-package gen_js_api           \
		-package gen_js_api.ppx       \
		-linkpkg $<

.PHONY:bindings

bindings:
	make -C bindings


clean:
	make -C bindings clean
	rm -f main.byte main.js main.cm*

