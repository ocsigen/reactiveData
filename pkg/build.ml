#!/usr/bin/env ocaml
#directory "pkg";;
#use "topkg.ml";;


let builder = `Other
    ( "ocamlbuild -use-ocamlfind -classic-display -plugin-tag \"package(js_of_ocaml.ocamlbuild)\"",
      "_build")

let () =
  Pkg.describe "reactiveList" ~builder [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.module_library "src/reactiveData";
    Pkg.lib ~exts:Exts.module_library "src/reactiveDomList" ]
