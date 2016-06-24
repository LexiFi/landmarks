.PHONY: all configure download

all:
	@echo "Available rules:"
	@echo "  - download: get the source of coq.8.5.1 and patch the build system"
	@echo "  - configure: calls './configure -local'"
	@echo "  - build: builds the coqbinaries with automatically instrumented code and build the coqlibraries the instrumented coq"
	@echo "  - distclean: cleans everything"

download: coq.8.5.1

coq.8.5.1:
	opam source coq.8.5.1
	patch coq.8.5.1/Makefile.build < Makefile.build.patch
	sed -i 's/"threads.cma"/&;"landmarks-threads.cma"/' coq.8.5.1/tools/coqmktop.ml 

configure:
	cd coq.8.5.1; ./configure -local

build:
	cd coq.8.5.1; OCAML_LANDMARKS=auto,threads,off make coqbinaries VERBOSE=1
	cd coq.8.5.1; OCAML_LANDMARKS=auto,threads,output=temporary,format=json make coqlib VERBOSE=1

distclean:
	rm -rf coq.8.5.1
