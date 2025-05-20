.PHONY: run
run:
	dune exec ./bin/main.exe

.PHONY: build
build:
	dune build

.PHONY: fmt
fmt:
	dune build @fmt

