LANDMARKS_SRC?=$(shell ocamlfind query landmarks)
ARCHIVE?=$(shell opam config var switch)
VERSION?=$(shell opam config var ocaml-version)

all:
	@cat README

configure: ocaml-$(ARCHIVE)
	mkdir -p local
	cd ocaml-$(ARCHIVE); ./configure -prefix $(shell readlink -f local); echo "LANDMARKS_SRC=$(LANDMARKS_SRC)" >> config/Makefile
        
build:
	OCAML_LANDMARKS=auto,off make -C ocaml-$(ARCHIVE) world world.opt

install:
	make -C ocaml-$(ARCHIVE) install

ocaml-$(ARCHIVE): $(ARCHIVE).tar.gz
	tar -xzf $(ARCHIVE).tar.gz
	cd $@; patch -p 1 < ../landmarks_build-$(VERSION).patch

$(ARCHIVE).tar.gz:
	wget --no-check-certificate https://github.com/ocaml/ocaml/archive/$(ARCHIVE).tar.gz

clean:
	make -C ocaml-$(ARCHIVE) clean

tests: install
	make -C tests

everything: configure build install tests

distclean:
	make -C tests clean
	rm -rf ocaml-$(ARCHIVE) $(ARCHIVE).tar.gz local
