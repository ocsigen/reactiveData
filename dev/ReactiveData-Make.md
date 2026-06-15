
# Module `ReactiveData.Make`

Functor for turning a plain container into an incremental one


## Parameters

```ocaml
module D : DATA
```

## Signature

```ocaml
type 'a t
```
Reactive version of the data container

```ocaml
type 'a data = 'a D.data
```
Raw (non-reactive) version of the data container

```ocaml
type 'a patch = 'a D.patch
```
Patch format

```ocaml
type 'a msg = 
  | Patch of 'a patch (* Patch p triggers the application of p on the current contents *)
  | Set of 'a data (* With Set d, d becomes the new content *)
```
Message format

```ocaml
type 'a handle
```
Handle that permits applying incremental updates

```ocaml
val empty : 'a t
```
Empty data structure

```ocaml
val create : 'a data -> 'a t * 'a handle
```
Build a container from initial contents. The handle can be used for performing reactive updates.

```ocaml
val from_event : 'a data -> 'a msg React.E.t -> 'a t
```
`from_event d e` is a container whose initial value is `d`, and which gets updated for every occurrence of `e`

```ocaml
val from_signal : ?eq:('a -> 'a -> bool) -> 'a data React.S.t -> 'a t
```
Convert a React signal into a ReactiveData container.

Whenever the signal changes from value `v` to value `v'`, we detect the differences between `v` and `v'`, and perform downstream computation (e.g., for `map`) only on the new and modified elements.

```ocaml
val const : 'a data -> 'a t
```
Produce a constant container

```ocaml
val patch : 'a handle -> 'a patch -> unit
```
`patch h p` applies `p` on the container corresponding to `h`

```ocaml
val set : 'a handle -> 'a data -> unit
```
`set h d` sets the contents of the container corresponding to `h`, disregarding previous contents

```ocaml
val map_msg : ('a -> 'b) -> 'a msg -> 'b msg
```
Transform a message

```ocaml
val map : ('a -> 'b) -> 'a t -> 'b t
```
`map f c` applies `f` on all elements of `c`, producing a new reactive container `c'`. Modifying the contents of `c` leads to modifications of `c'`. `f` is applied only on the new or modified elements of `c`.

```ocaml
val value : 'a t -> 'a data
```
Return current contents

```ocaml
val fold : ('a -> 'b msg -> 'a) -> 'b t -> 'a -> 'a React.signal
```
`fold f c v` accumulates the updates on `c` with `f` starting from `v`.

The result is a signal of value `f m_n (f ... (f m_1 v))`, where `m_1` ... `m_n` are the messages that have been applied since the beginning of `fold`. `m_1` is a pseudo-message `Set l`, accounting for the contents `l` of `c` at the time when accumulation starts.

```ocaml
val signal : ?eq:('a -> 'a -> bool) -> 'a t -> 'a data React.S.t
```
Signal corresponding to contents

```ocaml
val event : 'a t -> 'a msg React.E.t
```
Event whose occurrences correspond to container updates
