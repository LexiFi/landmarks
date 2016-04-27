.PHONY: all clean
all:
	@for dir in tests/*; do \
	  $(MAKE) -C $$dir; \
        done

clean:
	@for dir in tests/*; do \
	  $(MAKE) -C $$dir clean; \
        done
