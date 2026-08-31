
# Parameter `Make.D`

```ocaml
type 'a data
```
Data container

```ocaml
type 'a patch
```
Patch format for modifying the container

```ocaml
val merge : 'a patch -> 'a data -> 'a data
```
Applicative merge operation: `merge p d` is a new container produced by applying `p` on `d`. `d` does not change.

```ocaml
val map_patch : ('a -> 'b) -> 'a patch -> 'b patch
```
Transform a patch

```ocaml
val map_data : ('a -> 'b) -> 'a data -> 'b data
```
`map f d` applies `f` on all the elements of `d`, producing a new container in an applicative way

```ocaml
val empty : 'a data
```
Empty container

```ocaml
val equal : ('a -> 'a -> bool) -> 'a data -> 'a data -> bool
```
Lift an equality operator over atoms of type `'a` to an equality operator over `'a data`

```ocaml
val diff : eq:('a -> 'a -> bool) -> 'a data -> 'a data -> 'a patch
```
`diff ?eq d1 d2` produces a patch describing the differences between `d1` and `d2`.

The optional `?eq` argument is used for comparing the atoms inside `d1` and `d2`. (The default value for `eq` is `(=)`.)
