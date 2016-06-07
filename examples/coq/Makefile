coq.8.5.1:
	opam source coq.8.5.1

patch: coq.8.5.1
	patch coq.8.5.1/Makefile.build < Makefile.build.patch

configure:
	cd coq.8.5.1; ./configure -local

build:
	cd coq.8.5.1; OCAML_LANDMARKS=auto,threads,off make coqbinaries VERBOSE=1
	cd coq.8.5.1; OCAML_LANDMARKS=auto,threads,output=temporary,format=json make coqlib VERBOSE=1

distclean:
	rm -rf coq.8.5.1
