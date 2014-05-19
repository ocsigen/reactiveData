
module Patch : sig
  type 'a t =
    | I of int * 'a (* insert at pos *)
    | R of int (* remove at pos *)
    | A of 'a (* append *)
    | C of 'a (* cons *)
    | X of int * int (* swap two position *)
    | U of int * 'a (* update a position *)

  val merge : 'a t -> 'a list -> 'a list
end


type 'a t
type 'a handle
type 'a msg =
  | Patch of 'a Patch.t
  | Set of 'a list

val make : 'a list -> 'a t * 'a handle
val set : 'a list -> 'a handle -> unit
val map : ('a -> 'b) -> 'a t -> 'b t
val append : 'a -> 'a handle -> unit
val cons : 'a -> 'a handle -> unit
val insert : 'a -> int -> 'a handle -> unit
val remove : int -> 'a handle -> unit
val swap : int -> int -> 'a handle -> unit
val update : 'a -> int -> 'a handle -> unit
val sort : ('a -> 'a -> int) -> 'a handle -> unit

val fold : ('b -> 'a msg -> 'b) -> 'a t -> 'b -> 'b React.S.t
val list : 'a t -> 'a list React.S.t
val value : 'a t -> 'a list
