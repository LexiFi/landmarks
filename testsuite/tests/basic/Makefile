BASEDIR=../../..

all: test
	@./test 2> log.err > log.out && echo "$$(basename $$PWD) : \033[32mOK\033[0m" || echo "$$(basename $$PWD) : \033[31mKO\033[0m"

test: test.ml
	ocamlopt -I $(BASEDIR)/src -o test landmarks.cmxa test.ml
clean:
	rm -f test *.cm[xoi] *.o log.err log.out
