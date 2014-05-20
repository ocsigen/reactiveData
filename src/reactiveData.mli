module type DATA =
  sig
    type 'a d
    type 'a p
    val merge : 'a p -> 'a d -> 'a d
    val map_p : ('a -> 'b) -> 'a p -> 'b p
    val map_d : ('a -> 'b) -> 'a d -> 'b d
  end
module type S =
  sig
    type 'a d
    type 'a p
    type 'a msg = Patch of 'a p | Set of 'a d
    type 'a handle
    type 'a t
    val make : 'a d -> 'a t * 'a handle
    val patch : 'a handle -> 'a p -> unit
    val set : 'a handle -> 'a d -> unit
    val map_msg : ('a -> 'b) -> 'a msg -> 'b msg
    val map : ('a -> 'b) -> 'a t -> 'b t
    val value : 'a t -> 'a d
    val fold : ('a -> 'b msg -> 'a) -> 'b t -> 'a -> 'a React.signal
    val value_s : 'a t -> 'a d React.S.t
    val event : 'a t -> 'a msg React.E.t
  end
module Make :
  functor (D : DATA) ->
    sig
      type 'a d = 'a D.d
      type 'a p = 'a D.p
      type 'a msg = Patch of 'a p | Set of 'a d
      type 'a handle
      type 'a t
      val make : 'a d -> 'a t * 'a handle
      val patch : 'a handle -> 'a p -> unit
      val set : 'a handle -> 'a d -> unit
      val map_msg : ('a -> 'b) -> 'a msg -> 'b msg
      val map : ('a -> 'b) -> 'a t -> 'b t
      val value : 'a t -> 'a d
      val fold : ('a -> 'b msg -> 'a) -> 'b t -> 'a -> 'a React.signal
      val value_s : 'a t -> 'a d React.S.t
      val event : 'a t -> 'a msg React.E.t
    end
module DataList :
  sig
    type 'a d = 'a list
    type 'a p =
        I of int * 'a
      | R of int
      | U of int * 'a
      | X of int * int
    val map_d : ('a -> 'b) -> 'a list -> 'b list
    val map_p : ('a -> 'b) -> 'a p -> 'b p
    val merge : 'a p -> 'a list -> 'a list
  end
module RList :
  sig
    type 'a d = 'a DataList.d
    type 'a p = 'a DataList.p
    type 'a msg = 'a Make(DataList).msg = Patch of 'a p | Set of 'a d
    type 'a handle = 'a Make(DataList).handle
    type 'a t = 'a Make(DataList).t
    val make : 'a d -> 'a t * 'a handle
    val patch : 'a handle -> 'a p -> unit
    val set : 'a handle -> 'a d -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val value : 'a t -> 'a d
    val fold : ('a -> 'b msg -> 'a) -> 'b t -> 'a -> 'a React.signal
    val value_s : 'a t -> 'a d React.S.t
    val event : 'a t -> 'a msg React.E.t
    val append : 'a -> 'a handle -> unit
    val cons : 'a -> 'a handle -> unit
    val insert : 'a -> int -> 'a handle -> unit
    val remove : int -> 'a handle -> unit
    val update : 'a -> int -> 'a handle -> unit
    val swap : int -> int -> 'a handle -> unit
    val concat : 'a t -> 'a t -> 'a t
  end
module RMap :
  functor (M : Map.S) ->
    sig
      module Data :
        sig
          type 'a d = 'a M.t
          type 'a p = [ `Add of M.key * 'a | `Del of M.key ]
          val merge :
            [< `Add of M.key * 'a | `Del of M.key ] -> 'a M.t -> 'a M.t
          val map_p :
            ('a -> 'b) ->
            [< `Add of 'c * 'a | `Del of 'd ] ->
            [> `Add of 'c * 'b | `Del of 'd ]
          val map_d : ('a -> 'b) -> 'a M.t -> 'b M.t
        end
      type 'a d = 'a Data.d
      type 'a p = 'a Data.p
      type 'a msg = 'a Make(Data).msg = Patch of 'a p | Set of 'a d
      type 'a handle = 'a Make(Data).handle
      type 'a t = 'a Make(Data).t
      val make : 'a d -> 'a t * 'a handle
      val patch : 'a handle -> 'a p -> unit
      val set : 'a handle -> 'a d -> unit
      val map_msg : ('a -> 'b) -> 'a msg -> 'b msg
      val map : ('a -> 'b) -> 'a t -> 'b t
      val value : 'a t -> 'a d
      val fold : ('a -> 'b msg -> 'a) -> 'b t -> 'a -> 'a React.signal
      val value_s : 'a t -> 'a d React.S.t
      val event : 'a t -> 'a msg React.E.t
    end
module RArray :
  sig
    module Data :
      sig
        type 'a d = 'a array
        type 'a p = unit
        val merge : 'a -> 'b -> 'b
        val map_p : 'a -> 'b -> 'b
        val map_d : ('a -> 'b) -> 'a array -> 'b array
      end
    type 'a d = 'a Data.d
    type 'a p = 'a Data.p
    type 'a msg = 'a Make(Data).msg = Patch of 'a p | Set of 'a d
    type 'a handle = 'a Make(Data).handle
    type 'a t = 'a Make(Data).t
    val make : 'a d -> 'a t * 'a handle
    val patch : 'a handle -> 'a p -> unit
    val set : 'a handle -> 'a d -> unit
    val map_msg : ('a -> 'b) -> 'a msg -> 'b msg
    val map : ('a -> 'b) -> 'a t -> 'b t
    val value : 'a t -> 'a d
    val fold : ('a -> 'b msg -> 'a) -> 'b t -> 'a -> 'a React.signal
    val value_s : 'a t -> 'a d React.S.t
    val event : 'a t -> 'a msg React.E.t
  end
