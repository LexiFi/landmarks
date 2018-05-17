main.exe: main.cmx
	ocamlfind ocamlopt -o main.exe -package unix -package landmarks -linkpkg main.cmx

main.cmx: main.ml
	ocamlfind ocamlopt -o main.cmx -package unix -package landmarks -package landmarks.ppx -c main.ml

.PHONY: clean

clean:
	rm -f main.exe main.cm* main.o
