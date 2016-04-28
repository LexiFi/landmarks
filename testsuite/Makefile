.PHONY: native batch clean

native:
	@for dir in tests/*; do \
	  $(MAKE) -C $$dir native; \
        done

batch:
	@for dir in tests/*; do \
	  $(MAKE) -C $$dir batch; \
        done

clean:
	@for dir in tests/*; do \
	  $(MAKE) -C $$dir clean; \
        done
