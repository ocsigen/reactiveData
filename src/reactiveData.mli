(* ReactiveData
 * https://github.com/ocsigen/reactiveData
 * Copyright (C) 2014 Hugo Heuzard
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(** ReactiveData is a module for data-aware functional reactive programming
    (FRP). It adds support to incremental changes in data structures by
    reasoning on patches instead of absolute values. ReactiveData is based on
    and inter-operates with React.

    You are most likely interested in the sub-module [RList], which implements a
    superset of the signature [S]. *)

(** Signature describing a reactive data structure (['a t]).

    Most functions in [S] are not safe to call during a React update step. *)
module type S = sig
  type 'a t
  (** Reactive version of the data container *)

  type 'a data
  (** Raw (non-reactive) version of the data container *)

  type 'a patch
  (** Patch format *)

  (** Message format *)
  type 'a msg =
    | Patch of 'a patch
        (** [Patch p] triggers the application of [p] on the current contents *)
    | Set of 'a data  (** With [Set d], [d] becomes the new content *)

  type 'a handle
  (** Handle that permits applying incremental updates *)

  val empty : 'a t
  (** Empty data structure *)

  val create : 'a data -> 'a t * 'a handle
  (** Build a container from initial contents. The handle can be used for
      performing reactive updates. *)

  val from_event : 'a data -> 'a msg React.E.t -> 'a t
  (** [from_event d e] is a container whose initial value is [d], and which gets
      updated for every occurrence of [e] *)

  val from_signal : ?eq:('a -> 'a -> bool) -> 'a data React.S.t -> 'a t
  (** Convert a React signal into a ReactiveData container.

      Whenever the signal changes from value [v] to value [v'], we detect the
      differences between [v] and [v'], and perform downstream computation
      (e.g., for [map]) only on the new and modified elements. *)

  val const : 'a data -> 'a t
  (** Produce a constant container *)

  val patch : 'a handle -> 'a patch -> unit
  (** [patch h p] applies [p] on the container corresponding to [h] *)

  val set : 'a handle -> 'a data -> unit
  (** [set h d] sets the contents of the container corresponding to [h],
      disregarding previous contents *)

  val map_msg : ('a -> 'b) -> 'a msg -> 'b msg
  (** Transform a message *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map f c] applies [f] on all elements of [c], producing a new reactive
      container [c']. Modifying the contents of [c] leads to modifications of
      [c']. [f] is applied only on the new or modified elements of [c]. *)

  val value : 'a t -> 'a data
  (** Return current contents *)

  val fold : ('a -> 'b msg -> 'a) -> 'b t -> 'a -> 'a React.signal
  (** [fold f c v] accumulates the updates on [c] with [f] starting from [v].

      The result is a signal of value [f m_n (f ... (f m_1 v))], where [m_1] ...
      [m_n] are the messages that have been applied since the beginning of
      [fold]. [m_1] is a pseudo-message [Set l], accounting for the contents [l]
      of [c] at the time when accumulation starts. *)

  val signal : ?eq:('a -> 'a -> bool) -> 'a t -> 'a data React.S.t
  (** Signal corresponding to contents *)

  val event : 'a t -> 'a msg React.E.t
  (** Event whose occurrences correspond to container updates *)
end

(** Reactive list data structure *)
module RList : sig
  (** Patch operation on lists. All operations are of linear complexity. *)
  type 'a p =
    | I of int * 'a  (** [I (i, v)] adds [v] at position [i] *)
    | R of int  (** [R i] removes [i]-th element *)
    | U of int * 'a  (** [U (i, v)] substitutes [i]-th element with [v] *)
    | X of int * int  (** [X (i, j)] swaps the [i]-th and [j]-th elements *)

  type 'a patch = 'a p list
  (** A patch is a list of patch operations. The operations are applied in the
      order they appear in the list.

      The indices correspond to list contents after the operations that appear
      earlier in the list have been applied, not to the contents before the
      whole patch operation.

      A patch comprised of [I], [R], and [U] steps with increasing indices can
      be applied in time O(m + n), where m is the patch length and n is the
      current size of the list. (Arbitrary patches are slower, requiring O(m *
      n).) *)

  include S with type 'a data = 'a list and type 'a patch := 'a patch

  val cons : 'a -> 'a handle -> unit
  (** Add element to the beginning *)

  val snoc : 'a -> 'a handle -> unit
  (** Add element to the end *)

  val insert : 'a -> int -> 'a handle -> unit
  (** [insert v i h] adds [v] as the [i]-th position in the container
      corresponding to [h]. The indices of the subsequent elements change. *)

  val remove : int -> 'a handle -> unit
  (** [remove i h] removes the [i]-th position from the container corresponding
      to [h]. The indices of the subsequent elements change. *)

  val remove_last : 'a t * 'a handle -> unit
  (** [remove_last a] removes the last element of [a] *)

  val remove_eq : ?eq:('a -> 'a -> bool) -> 'a t * 'a handle -> 'a -> unit
  (** [remove_eq l x] removes the first occurence of [x] from [l] *)

  val update : 'a -> int -> 'a handle -> unit
  (** [update v i h] substitutes the [i]-th element of the container
      corresponding to [h] with [v] *)

  val update_eq : ?eq:('a -> 'a -> bool) -> 'a t * 'a handle -> 'a -> 'a -> unit
  (** [update_eq l a b] substitutes the first occurence of [a] (according to
      [eq]) in [l] with [b] *)

  val move : int -> int -> 'a handle -> unit
  (** [move i offset h] moves the [i]-th element of the container corresponding
      by [offset] positions in [h], modifying the indices of other elements *)

  val singleton : 'a -> 'a t
  (** Produce container list containing a single, constant element *)

  val singleton_s : 'a React.S.t -> 'a t
  (** Produce reactive list containing a single element that gets updated based
      on a signal *)

  val concat : 'a t -> 'a t -> 'a t
  (** [concat a b] is the concatenation of [a] and [b], and it gets updated
      whenever [a] and [b] change *)

  val rev : 'a t -> 'a t
  (** [rev a] is the reversal of [a]; [rev a] gets updated along with [a] *)

  val filter : ('a -> bool) -> 'a t -> 'a t
  (** [filter pred l] keeps the elements of [l] matching [pred]; gets updated
      when [l] is. [pred] should be a pure function *)

  val for_all : ('a -> bool) -> 'a t -> bool React.S.t
  (** [for_all fn l] is a [bool React.S.t] verifying that all elements [x] of
      [l] satisfy [fn x] *)
end

(** Reactive map data structure *)
module RMap (M : Map.S) : sig
  type 'a patch = [ `Add of M.key * 'a | `Del of M.key ] list

  include S with type 'a data = 'a M.t and type 'a patch := 'a patch

  val filter : (M.key -> 'a -> bool) -> 'a t -> 'a t
  (** [filter pred l] keeps the elements of [l] matching [pred]; gets updated
      when [l] is. [pred] should be a pure function *)
end

(** Signature describing a raw data container (['a data]).

    Given an implementation of [DATA], an incremental version of the container
    can be produced (via [Make]). *)
module type DATA = sig
  type 'a data
  (** Data container *)

  type 'a patch
  (** Patch format for modifying the container *)

  val merge : 'a patch -> 'a data -> 'a data
  (** Applicative merge operation: [merge p d] is a new container produced by
      applying [p] on [d]. [d] does not change. *)

  val map_patch : ('a -> 'b) -> 'a patch -> 'b patch
  (** Transform a patch *)

  val map_data : ('a -> 'b) -> 'a data -> 'b data
  (** [map f d] applies [f] on all the elements of [d], producing a new
      container in an applicative way *)

  val empty : 'a data
  (** Empty container *)

  val equal : ('a -> 'a -> bool) -> 'a data -> 'a data -> bool
  (** Lift an equality operator over atoms of type ['a] to an equality operator
      over ['a data] *)

  val diff : eq:('a -> 'a -> bool) -> 'a data -> 'a data -> 'a patch
  (** [diff ?eq d1 d2] produces a patch describing the differences between [d1]
      and [d2].

      The optional [?eq] argument is used for comparing the atoms inside [d1]
      and [d2]. (The default value for [eq] is [(=)].) *)
end

(** Functor for turning a plain container into an incremental one *)
module Make (D : DATA) :
  S with type 'a data = 'a D.data and type 'a patch = 'a D.patch
