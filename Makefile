.PHONY: landmarks tests clean

landmarks:
	@$(MAKE) -C src

tests: landmarks
	@echo ""
	@echo "\033[1;4mNATIVE TESTS\033[0m"
	@$(MAKE) --no-print-directory -C testsuite native
	@echo ""
	@echo "\033[1;4mBATCH TESTS\033[0m"
	@$(MAKE) --no-print-directory -C testsuite batch

clean:
	@$(MAKE) -C src clean
	@$(MAKE) -C testsuite clean
