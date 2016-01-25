.PHONY: test

build:
	ocaml pkg/build.ml native=true native-dynlink=true

clean:
	ocamlbuild -clean

doc: build
	ocamlbuild -use-ocamlfind src/api.docdir/index.html


NAME=reactiveData

doc/html/.git:
	mkdir -p doc/html
	cd doc/html && (\
		git init && \
		git remote add origin git@github.com:ocsigen/$(NAME).git && \
		git checkout -b gh-pages \
	)

gh-pages: doc/html/.git doc
	cd doc/html && git checkout gh-pages
	rm -f doc/html/dev/*
	cp _build/src/api.docdir/*.html doc/html/dev/
	cd doc/html && git add *.html
	cd doc/html && git commit -a -m "Doc updates"
	cd doc/html && git push origin gh-pages
