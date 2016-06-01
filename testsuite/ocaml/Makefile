BASEDIR:=$(shell ocamlfind query landmarks)
OCAMLPARAM="I=$(BASEDIR),cmxa=landmarks.cmxa,ppx=$(BASEDIR)/ppx_landmarks,_"

all:
	@echo "Warning: this work only with the ocamlparam branch"
	@echo ""
	@echo "make configure: download and bootstrap ocaml"
	@echo "make world.opt: compile with landmarks"
	@echo "make install: install locally"
	@echo ""
	@echo "Useful for debugging:"
	@echo "export OCAMLPARAM=\"$(OCAMLPARAM)\" OCAML_LANDMARKS=auto"


configure: ocaml-ocamlparam
	mkdir -p local
	cd ocaml-ocamlparam; unset OCAMLPARAM; unset OCAML_LANDMARKS; ./configure -prefix $(shell readlink -f local); make world bootstrap

world.opt:
	cp arch.mli ocaml-ocamlparam/asmcomp
	cd ocaml-ocamlparam; unset OCAMLPARAM; unset OCAML_LANDMARKS; make depend world checkstack runtime core ocaml opt-core; OCAMLPARAM=$(OCAMLPARAM) OCAML_LANDMARKS="auto" make ocamlc.opt otherlibraries ocamlopt.opt otherlibrariesopt ocamllex.opt ocamltoolsopt ocamltoolsopt.opt

install: world.opt
	cd ocaml-ocamlparam; make install

ocaml-ocamlparam: ocamlparam.tar.gz
	tar -xzf ocamlparam.tar.gz

ocamlparam.tar.gz:
	wget --no-check-certificate https://github.com/mlasson/ocaml/archive/ocamlparam.tar.gz

clean:
	make -C ocaml-ocamlparam clean

distclean:
	make -C tests clean
	rm -rf ocaml-ocamlparam ocamlparam.tar.gz local
