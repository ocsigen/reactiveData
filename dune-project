(lang dune 3.19)
(name reactiveData)

(generate_opam_files true)

(maintainers "Ocsigen team <dev@ocsigen.org>")
(authors "Hugo Heuzard <hugo.heuzard@gmail.com>")
(license "LGPL-3.0-or-later WITH OCaml-LGPL-linking-exception")
(source (github ocsigen/reactiveData))

(package
 (name reactiveData)
 (synopsis "Declarative events and signals for OCaml")
 (description "React is an OCaml module for functional reactive programming (FRP). It provides support to program with time varying values : declarative events and signals. React doesn't define any primitive event or signal, it lets the client chooses the concrete timeline.")
 (maintenance_intent "(latest)")
 (depends
  (ocaml (>= 4.08))
  (react (>= 1.2.1)))
 (tags ("reactive" "declarative" "signal" "event" "frp")))
