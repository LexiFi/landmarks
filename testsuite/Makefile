.PHONY: native batch clean all

all: native batch

native:
	@for dir in tests/*; do \
	  $(MAKE) --no-print-directory -C $$dir native; \
        done

batch:
	@for dir in tests/*; do \
	  $(MAKE) --no-print-directory -C $$dir batch; \
        done

clean:
	@for dir in tests/*; do \
	  $(MAKE) --no-print-directory -C $$dir clean; \
        done
