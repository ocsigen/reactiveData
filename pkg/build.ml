#!/usr/bin/env ocaml
#directory "pkg";;
#use "topkg.ml";;

let () =
  Pkg.describe "reactiveList" ~builder:`OCamlbuild [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.module_library "src/reactiveList";
    Pkg.lib ~exts:Exts.module_library "src/reactiveDomList" ]
