.PHONY: native batch clean all

all: native batch

native:
	@for dir in tests/*; do \
	  $(MAKE) --no-print-directory -C $$dir native; \
        done
	@find . -name "*.err" -exec grep -H '\[LANDMARKS\]' '{}' ';'

batch:
	@for dir in tests/*; do \
	  $(MAKE) --no-print-directory -C $$dir batch; \
        done
	@find . -name "*.err" -exec grep -H '\[LANDMARKS\]' '{}' ';'

check_failure:
	test -z "`find tests -name failed`" || (echo "Some test(s) failed."; exit 1)

clean:
	@for dir in tests/*; do \
	  $(MAKE) --no-print-directory -C $$dir clean; \
        done
