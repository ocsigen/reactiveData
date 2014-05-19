.PHONY: test

build:
	ocaml pkg/build.ml native=true native-dynlink=true

test:
	ocamlbuild -use-ocamlfind -plugin-tag "package(js_of_ocaml.ocamlbuild)" test/test.js

clean:
	ocamlbuild -clean
