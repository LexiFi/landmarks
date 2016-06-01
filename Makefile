PACKAGE=landmarks
VERSION=1.0

.PHONY: landmarks tests clean ppx all tools check_ocamlfind

all: landmarks ppx META tools

check_ocamlfind:
	@ocamlfind printconf > /dev/null 2>&1 || echo "Findlib is required to build this package."

META: Makefile
	@sed -i "s/version = \"[^\"]*\"/version = \"$(VERSION)\"/" $@

landmarks: check_ocamlfind
	@$(MAKE) --no-print-directory -C src

ppx: check_ocamlfind
	@$(MAKE) --no-print-directory -C ppx

tools: check_ocamlfind
	@ocamlfind query gen_js_api > /dev/null \
	&& $(MAKE) --no-print-directory -C tools/landmarks_viewer \
	|| echo '[WARNING] The package `gen_js_api` is required to build the landmarks viewer.' 

tests: landmarks
	@ocamlfind query ppx_tools > /dev/null || (echo '[ERROR] The package `ppx_tools` is required to run the testsuite.' && exit 1)
	@echo ""
	@echo "\033[1;4mNATIVE TESTS\033[0m"
	@$(MAKE) --no-print-directory -C testsuite native
	@echo ""
	@echo "\033[1;4mBATCH TESTS\033[0m"
	@$(MAKE) --no-print-directory -C testsuite batch

clean:
	@$(MAKE) -C src clean
	@$(MAKE) -C ppx clean
	@$(MAKE) -C testsuite clean
	@$(MAKE) -C tools/landmarks_viewer clean

.PHONY: install uninstall

INSTALL=META $(wildcard src/*.cmo) \
             $(wildcard src/*.cma) \
             $(wildcard src/*.cmi) \
             $(wildcard src/*.cmt) \
             $(wildcard src/*.cmti) \
             $(wildcard src/*.mli) \
             $(wildcard src/*.cmx) \
             $(wildcard src/*.cmxa) \
             $(wildcard src/*.cmxs) \
             $(wildcard src/*.a) \
             $(wildcard src/*.o) \
             $(wildcard src/*.so) \
             $(wildcard ppx/ppx_landmarks) \
             $(wildcard tools/landmarks_viewer/landmarks_viewer.html) \
             $(wildcard tools/landmarks_viewer/landmarks_viewer.js)

install: check_ocamlfind
	ocamlfind install $(PACKAGE) $(INSTALL)

uninstall: check_ocamlfind
	ocamlfind remove $(PACKAGE)
