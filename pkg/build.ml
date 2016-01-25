#!/usr/bin/env ocaml
#directory "pkg";;
#use "topkg.ml";;

let () =
  Pkg.describe "reactiveData" ~builder:`OCamlbuild [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.module_library "src/reactiveData";

    Pkg.doc "README.md";
    Pkg.doc "CHANGES";
  ]
