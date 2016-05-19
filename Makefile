.PHONY: landmarks tests clean ppx all tools

all: landmarks ppx

landmarks:
	@$(MAKE) -C src

ppx:
	@$(MAKE) -C ppx

tools:
	@$(MAKE) -C tools/js_graphviewer

tests: landmarks
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
