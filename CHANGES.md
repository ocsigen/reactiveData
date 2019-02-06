## 0.2.3

* port to dune from ocamlbuild

## 0.2.2

* additional functions in RList and RMap
* do not use a generic hashtable as it does not work with functional values

## 0.2.1

* Add ?eq parameter to S.signal

## 0.2

* Add `from_signal`, which converts React signals to ReactiveData
  containers. `from_signal` uses a diffing algorithm to detect what
  changes, thus minimizing the updates needed downstream.
* Optimize common cases of `merge`.
* Provide documentation and make the naming more consistent.

## 0.1

* First public release
