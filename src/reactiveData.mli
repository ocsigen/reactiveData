(* ReactiveData
 * https://github.com/hhugo/reactiveData
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

module type DATA = sig
  type 'a data
  type 'a patch
  val merge : 'a patch -> 'a data -> 'a data
  val map_patch : ('a -> 'b) -> 'a patch -> 'b patch
  val map_data : ('a -> 'b) -> 'a data -> 'b data
  val empty : 'a data
  val equal : ('a -> 'a -> bool) -> 'a data -> 'a data -> bool
  val diff : 'a data -> 'a data -> eq:('a -> 'a -> bool) -> 'a patch
end
module type S = sig
  type 'a data
  type 'a patch
  type 'a msg = Patch of 'a patch | Set of 'a data
  type 'a handle
  type 'a t
  val empty : 'a t
  val make : ?eq:('a -> 'a -> bool) -> 'a data -> 'a t * 'a handle
  val make_from :
    ?eq:('a -> 'a -> bool) -> 'a data -> 'a msg React.E.t -> 'a t
  val make_from_s :
    ?eq:('a -> 'a -> bool) -> 'a data React.S.t -> 'a t
  val const : 'a data -> 'a t
  val patch : 'a handle -> 'a patch -> unit
  val set : 'a handle -> 'a data -> unit
  val map_msg : ('a -> 'b) -> 'a msg -> 'b msg
  val map : ?eq:('b -> 'b -> bool) -> ('a -> 'b) -> 'a t -> 'b t
  val value : 'a t -> 'a data
  val fold :
    ?eq:('a -> 'a -> bool) ->
    ('a -> 'b msg -> 'a) ->
    'b t -> 'a -> 'a React.signal
  val value_s : 'a t -> 'a data React.S.t
  val event : 'a t -> 'a msg React.E.t
end

module Make(D : DATA) : S with type 'a data = 'a D.data
                           and type 'a patch = 'a D.patch
module RList :
sig
  type 'a p =
      I of int * 'a
    | R of int
    | U of int * 'a
    | X of int * int
  include S with type 'a data = 'a list
             and type 'a patch = 'a p list

  val nil : 'a t
  val append : 'a -> 'a handle -> unit
  val cons : 'a -> 'a handle -> unit
  val insert : 'a -> int -> 'a handle -> unit
  val remove : int -> 'a handle -> unit
  val update : 'a -> int -> 'a handle -> unit
  val move : int -> int -> 'a handle -> unit

  val singleton : 'a -> 'a t
  val singleton_s : 'a React.S.t -> 'a t
  val concat : 'a t -> 'a t -> 'a t
  val rev : 'a t -> 'a t
  val sort : ('a -> 'a -> int) -> 'a t -> [`Not_implemented]
  val filter : ('a -> unit) -> 'a t -> [`Not_implemented]
end

module RMap(M : Map.S) : S
  with type 'a data = 'a M.t
   and type 'a patch = [ `Add of M.key * 'a | `Del of M.key ] list
