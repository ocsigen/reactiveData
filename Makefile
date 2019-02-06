.PHONY: build clean doc test

build:
	dune build

test:
	dune runtest

clean:
	dune clean

doc:
	dune build @doc
