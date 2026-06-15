
# Module `ReactiveData`

ReactiveData is a module for data-aware functional reactive programming (FRP). It adds support to incremental changes in data structures by reasoning on patches instead of absolute values. ReactiveData is based on and inter-operates with React.

You are most likely interested in the sub-module `RList`, which implements a superset of the signature `S`.

```ocaml
module type S = sig ... end
```
Signature describing a reactive data structure (`'a t`).

```ocaml
module RList : sig ... end
```
Reactive list data structure

```ocaml
module RMap (M : Stdlib.Map.S) : sig ... end
```
Reactive map data structure

```ocaml
module type DATA = sig ... end
```
Signature describing a raw data container (`'a data`).

```ocaml
module Make
  (D : DATA) : 
  S with type 'a data = 'a D.data and type 'a patch = 'a D.patch
```
Functor for turning a plain container into an incremental one
