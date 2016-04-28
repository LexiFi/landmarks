landmarks:
	@$(MAKE) -C src

tests:
	@$(MAKE) --no-print-directory -C testsuite
