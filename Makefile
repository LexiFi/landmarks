PACKAGE=landmarks
VERSION=1.1

.PHONY: landmarks tests clean ppx all tools check_ocamlfind doc

all: landmarks ppx META tools

META: Makefile
	@sed -i "s/version = \"[^\"]*\"/version = \"$(VERSION)\"/" $@

doc: landmarks
	@$(MAKE) --no-print-directory doc -C src

landmarks:
	@$(MAKE) --no-print-directory -C src

ppx:
	@$(MAKE) --no-print-directory -C ppx

tools:
	@ocamlfind query gen_js_api > /dev/null 2>&1 \
	&& $(MAKE) --no-print-directory -C tools/landmarks_viewer \
	|| echo '[WARNING] Findlib and the package `gen_js_api` are required to build the landmarks viewer.' 

tests: landmarks
	@ocamlfind printconf > /dev/null 2>&1 || echo "Findlib is required to run tests."
	@ocamlfind query ppx_tools > /dev/null || (echo '[ERROR] The package `ppx_tools` is required to run the testsuite.' && exit 1)
	@echo ""
	@echo "\033[1;4mBATCH TESTS\033[0m"
	@$(MAKE) --no-print-directory -C testsuite batch
	@echo ""
	@echo "\033[1;4mNATIVE TESTS\033[0m"
	@$(MAKE) --no-print-directory -C testsuite native
	@$(MAKE) --no-print-directory -C testsuite check_failure

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
