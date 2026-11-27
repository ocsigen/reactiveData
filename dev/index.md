# ReactiveData

ReactiveData brings functional reactive programming to data structures that change by small steps. Built on [React](https://erratique.ch/software/react), it represents the changes of a list or a map as patches (insert this element, remove that one) rather than as successive whole values, and the structures computed from it are updated with the same patches: a change costs work in proportion to its size, not to the size of the structure.

It is the basis of the reactive HTML of [TyXML](https://ocsigen.org/tyxml/) and [Eliom](https://ocsigen.org/eliom/), where an element whose children come from a reactive list is updated in the page, child by child, when the list changes.

## Overview

- [`ReactiveData.RList`](./ReactiveData-RList.md): reactive lists, the module you most likely need. [`ReactiveData.RList.create`](./ReactiveData-RList.md#val-create) returns a list and a handle to change it, with [`ReactiveData.RList.cons`](./ReactiveData-RList.md#val-cons), [`ReactiveData.RList.snoc`](./ReactiveData-RList.md#val-snoc), [`ReactiveData.RList.insert`](./ReactiveData-RList.md#val-insert), [`ReactiveData.RList.remove`](./ReactiveData-RList.md#val-remove) or [`ReactiveData.RList.update`](./ReactiveData-RList.md#val-update). [`ReactiveData.RList.map`](./ReactiveData-RList.md#val-map), [`ReactiveData.RList.filter`](./ReactiveData-RList.md#val-filter), [`ReactiveData.RList.concat`](./ReactiveData-RList.md#val-concat) and [`ReactiveData.RList.rev`](./ReactiveData-RList.md#val-rev) derive other reactive lists from it.
- [`ReactiveData.RMap`](./ReactiveData-RMap.md): reactive maps, over any `Map.S`.
- [`ReactiveData.Make`](./ReactiveData-Make.md): the reactive version of another data structure, given its patch format ([`ReactiveData.DATA`](./ReactiveData-module-type-DATA.md)).
Reactive structures and React signals convert into each other: [`ReactiveData.RList.from_signal`](./ReactiveData-RList.md#val-from_signal) computes the patches between the successive values of a signal, and [`ReactiveData.RList.signal`](./ReactiveData-RList.md#val-signal) is the signal of the current contents.

## Example

```ocaml
let l, h = ReactiveData.RList.create [ 1; 2; 3 ]
let squares = ReactiveData.RList.map (fun x -> x * x) l

(* Adding 4 at the end sends the patch to [squares], which computes the
   square of 4 only. *)
let () = ReactiveData.RList.snoc 4 h
let () = assert (ReactiveData.RList.value squares = [ 1; 4; 9; 16 ])
```
