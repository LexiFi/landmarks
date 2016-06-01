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

clean:
	@for dir in tests/*; do \
	  $(MAKE) --no-print-directory -C $$dir clean; \
        done
