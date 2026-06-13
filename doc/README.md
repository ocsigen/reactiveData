# How the ReactiveData documentation is generated

The ReactiveData documentation published at <https://ocsigen.org/reactiveData/>
is built with **odoc** and themed with the Ocsigen site chrome by
[**wodoc**](https://github.com/ocsigen/wodoc) (an odoc driver). The same odoc
sources are also what ocaml.org renders.

## Sources

| What | Where | Format |
|---|---|---|
| API | the `.mli` of `reactiveData` (the `ReactiveData` module) | odoc comments |
| Site configuration (nav, …) | [`doc/wodoc`](wodoc) | wodoc config (S-expression) |

ReactiveData is a single-module library with no manual, so a plain
`dune build @doc --profile release` builds the API. The page theming and the
left navigation are declared in [`doc/wodoc`](wodoc) — an S-expression with the
project metadata, the `(nav …)` left menu, and `(manual-root)` (single-package:
deploy at the version root, so URLs are `/reactiveData/<version>/…` rather than
`/reactiveData/<version>/reactiveData/…`). See the
[wodoc README](https://github.com/ocsigen/wodoc) for the config syntax.

## Build

```
wodoc build --config doc/wodoc --label dev --out _doc-site/dev \
  --menu https://ocsigen.org/doc/menu.html
```

`wodoc build` runs `dune build @doc`, assembles every page into the Ocsigen site
(shared header/menu/drawer, the version `<select>`, the left navigation from
`doc/wodoc`). `--menu` is fetched from its single canonical copy in
`ocsigen.github.io`. Add `--local` to also fetch the shared `/css//img/` assets
and preview offline.

## Deployment (CI)

[`.github/workflows/doc.yml`](../.github/workflows/doc.yml) builds and publishes
to the project's **`gh-pages`** branch (served at `ocsigen.org/reactiveData/`).
On **push to `master`** it rebuilds and deploys the **`dev`** docs only. Each run
replaces only the `dev/` directory; the other version directories already on
`gh-pages` are preserved.

## Releasing a stable version

The CI builds only `dev/`. To publish a stable version, run the **Documentation**
workflow manually (GitHub → Actions → *Documentation* → "Run workflow") with the
**version** input (e.g. `0.3.1`). The `release` job freezes the current `dev/`
docs as `/<version>/`, repoints the `latest` symlink, writes the root redirect
and refreshes `versions.json` — via `wodoc release --from dev --version <version>`.
No rebuild: the docs of a release are exactly the `dev` docs at that point.

Equivalently, by hand on a `gh-pages` checkout:

```
wodoc release --site . --from dev --version <version>
git add -A && git commit -m "Release doc <version>" && git push
```
