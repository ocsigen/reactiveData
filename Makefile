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
	cp _build/src/api.docdir/* doc/html/dev/
	cd doc/html && git add dev
	cd doc/html && git commit -a -m "Doc updates"
	cd doc/html && git push origin gh-pages

release: doc/html/.git
	@if [ -z "$(VERSION)" ]; then echo "Usage: make release VERSION=1.0.0"; exit 1; fi
	cd doc/html && cp -r dev $(VERSION)
	cd doc/html && git add $(VERSION)
	cd doc/html && git commit -m "Doc updates $(VERSION)"
	cd doc/html && git push origin gh-pages
	sed -i "s/= dev =/= $(VERSION) =/" CHANGES
	git add CHANGES
	git commit -m "Version $(VERSION)."
	git tag -a $(VERSION) -m "Version $(VERSION)."

archive:
	@if [ -z "$(VERSION)" ]; then echo "Usage: make archive VERSION=1.0.0"; exit 1; fi
	wget "https://github.com/mirage/$(NAME)/archive/$(VERSION).tar.gz"
