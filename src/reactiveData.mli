module type DATA = sig
  type 'a data
  type 'a patch
  val merge : 'a patch -> 'a data -> 'a data
  val map_patch : ('a -> 'b) -> 'a patch -> 'b patch
  val map_data : ('a -> 'b) -> 'a data -> 'b data
end
module type S = sig
  type 'a data
  type 'a patch
  type 'a msg = Patch of 'a patch | Set of 'a data
  type 'a handle
  type 'a t
  val make : 'a data -> 'a t * 'a handle
  val make_from : 'a data -> 'a msg React.E.t -> 'a t
  val patch : 'a handle -> 'a patch -> unit
  val set : 'a handle -> 'a data -> unit
  val map_msg : ('a -> 'b) -> 'a msg -> 'b msg
  val map : ('a -> 'b) -> 'a t -> 'b t
  val value : 'a t -> 'a data
  val fold : ('a -> 'b msg -> 'a) -> 'b t -> 'a -> 'a React.signal
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
  val append : 'a -> 'a handle -> unit
  val cons : 'a -> 'a handle -> unit
  val insert : 'a -> int -> 'a handle -> unit
  val remove : int -> 'a handle -> unit
  val update : 'a -> int -> 'a handle -> unit
  val move : int -> int -> 'a handle -> unit
  val concat : 'a t -> 'a t -> 'a t
  val rev : 'a t -> 'a t
  val sort : ('a -> 'a -> int) -> 'a t -> 'a t
  val filter : ('a -> unit) -> 'a t -> 'a t
end
module RMap(M : Map.S) : S with type 'a data = 'a M.t
                            and type 'a patch = [ `Add of M.key * 'a | `Del of M.key ]
module RArray : S with type 'a data = 'a array
                   and type 'a patch = unit
