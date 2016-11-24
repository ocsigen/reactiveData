.PHONY: test

build:
	ocaml pkg/build.ml native=true native-dynlink=true

clean:
	ocamlbuild -clean

doc: build
	ocamlbuild -use-ocamlfind src/api.docdir/index.html


NAME=reactiveData
TMP=/tmp

TMP_REPO=$(TMP)/$(NAME)

$(TMP_REPO)/.git:
	cd $(TMP) && \
	git clone https://github.com/ocsigen/$(NAME).git && \
	cd $(NAME) && \
	git checkout gh-pages

gh-pages: $(TMP_REPO)/.git doc
	cd $(TMP_REPO) && git checkout gh-pages
	rm -f $(TMP_REPO)/dev/*
	cp _build/src/api.docdir/* $(TMP_REPO)/dev/
	cd $(TMP_REPO) && git add dev
	cd $(TMP_REPO) && git commit -a -m "Doc updates"
	cd $(TMP_REPO) && git push origin gh-pages

release: $(TMP_REPO)/.git
	@if [ -z "$(VERSION)" ]; then echo "Usage: make release VERSION=1.0.0"; exit 1; fi
	cd $(TMP_REPO) && cp -r dev $(VERSION)
	cd $(TMP_REPO) && git add $(VERSION)
	cd $(TMP_REPO) && git commit -m "Doc updates $(VERSION)"
	cd $(TMP_REPO) && git push origin gh-pages
	sed -i "s/= dev =/= $(VERSION) =/" CHANGES
	git add CHANGES
	git commit -m "Version $(VERSION)."
	git tag -a $(VERSION) -m "Version $(VERSION)."

prepare:
	@if [ -z "$(VERSION)" ]; then echo "Usage: make prepare VERSION=1.0.0"; exit 1; fi
	opam publish prepare "$(NAME).$(VERSION)" \
    "https://github.com/ocsigen/$(NAME)/archive/$(VERSION).tar.gz"
