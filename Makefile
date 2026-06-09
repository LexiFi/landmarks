.PHONY: build
build:
	dune build

.PHONY: test
test:
	dune test

# Speedscope demo and sanity check
.PHONY: speedscope-demo
speedscope-demo:
	dune build tests/speedscope/example.exe
	OCAML_LANDMARKS="format=speedscope,output=profile.speedscope,time" \
	  ./_build/default/tests/speedscope/example.exe
	@echo "👉 Upload profile.speedscope at https://www.speedscope.app/"
