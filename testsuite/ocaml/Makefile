BASEDIR:=$(shell ocamlfind query landmarks)
OCAMLPARAM="I=$(BASEDIR),cma=landmarks.cma,cmxa=landmarks.cmxa,ppx=$(BASEDIR)/ppx_landmarks,_"

all:
	@echo "Warning: this works only with the ocamlparam branch"
	@echo ""
	@echo "make configure: download and bootstrap ocaml"
	@echo "make world.opt: compile with landmarks"
	@echo "make install: install locally"
	@echo ""
	@echo "Useful for debugging:"
	@echo "export OCAMLPARAM=\"$(OCAMLPARAM)\" OCAML_LANDMARKS=auto"

configure: ocaml-ocamlparam
	mkdir -p local
	cd ocaml-ocamlparam; unset OCAMLPARAM; unset OCAML_LANDMARKS; ./configure -prefix $(shell readlink -f local)
	make -C ocaml-ocamlparam world bootstrap clean # The bootstrap is needed to be able to use OCAMLPARAM

world: ocaml-ocamlparam
	OCAMLPARAM=$(OCAMLPARAM) OCAML_LANDMARKS="auto,off" make -C ocaml-ocamlparam runtime ocamlc ocamllex ocamlyacc ocamltools
	OCAMLPARAM=$(OCAMLPARAM) OCAML_LANDMARKS="off" make -C ocaml-ocamlparam library otherlibraries # the landmark library depends on the stdlib, we cannot benchmark it easily
	OCAMLPARAM=$(OCAMLPARAM) OCAML_LANDMARKS="auto,off" make -C ocaml-ocamlparam ocaml
	OCAMLPARAM=$(OCAMLPARAM) OCAML_LANDMARKS="off" make -C ocaml-ocamlparam otherlibraries # the landmark library depends on the stdlib, we cannot benchmark it easily

world.opt: world
	OCAMLPARAM=$(OCAMLPARAM) OCAML_LANDMARKS="auto,off" make -C ocaml-ocamlparam runtimeopt ocamlopt
	OCAMLPARAM=$(OCAMLPARAM) OCAML_LANDMARKS="off" make -C ocaml-ocamlparam libraryopt  # the landmark library depends on the stdlib, we cannot benchmark it easily
	OCAMLPARAM=$(OCAMLPARAM) OCAML_LANDMARKS="auto,off" make -C ocaml-ocamlparam world.opt

install: world.opt
	cd ocaml-ocamlparam; make install

ocaml-ocamlparam: ocamlparam.tar.gz
	tar -xzf ocamlparam.tar.gz
	find ocaml-ocamlparam -name "arch.ml" -exec sed -i '1s/^/[@@@landmark "auto-off"]/' '{}' ';'
	find ocaml-ocamlparam -name "branch_relaxation_intf.ml" -exec sed -i '1s/^/[@@@landmark "auto-off"]/' '{}' ';'

ocamlparam.tar.gz:
	wget --no-check-certificate https://github.com/mlasson/ocaml/archive/ocamlparam.tar.gz

clean:
	make -C ocaml-ocamlparam clean

tests: install
	make -C tests

distclean:
	make -C tests clean
	rm -rf ocaml-ocamlparam ocamlparam.tar.gz local
