.PHONY: test

build:
	ocaml pkg/build.ml native=true native-dynlink=true

clean:
	ocamlbuild -clean

doc: build
	ocamlbuild -use-ocamlfind src/api.docdir/index.html
