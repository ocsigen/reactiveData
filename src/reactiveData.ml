module type DATA = sig
  type 'a d
  type 'a p
  val merge : 'a p -> 'a d -> 'a d
  val map_p : ('a -> 'b) -> 'a p -> 'b p
  val map_d : ('a -> 'b) -> 'a d -> 'b d
end
module type S = sig
  type 'a d
  type 'a p
  type 'a msg = Patch of 'a p | Set of 'a d
  type 'a handle
  type 'a t
  val make : 'a d -> 'a t * 'a handle
  val patch : 'a handle -> 'a p -> unit
  val set   : 'a handle -> 'a d -> unit
  val map_msg : ('a -> 'b) -> 'a msg -> 'b msg
  val map : ('a -> 'b) -> 'a t -> 'b t
  val value : 'a t -> 'a d
  val fold : ('a -> 'b msg -> 'a) -> 'b t -> 'a -> 'a React.signal
  val value_s : 'a t -> 'a d React.S.t
  val event : 'a t -> 'a msg React.E.t
end
module Make(D : DATA) :
  S with type 'a d = 'a D.d
     and type 'a p = 'a D.p = struct

  type 'a d = 'a D.d
  type 'a p = 'a D.p
  let merge = D.merge
  let map_p = D.map_p
  let map_d = D.map_d

  type 'a msg =
    | Patch of 'a p
    | Set of 'a d

  type 'a handle = 'a msg -> unit

  type 'a t = {
    current : 'a d ref;
    event : 'a msg React.E.t;
  }

  let make l =
    let initial_event,send = React.E.create () in
    let current = ref l in
    let event = React.E.map (fun msg ->
        begin match msg with
          | Set l -> current := l;
          | Patch p -> current := merge p !current
        end;
        msg) initial_event in
    {current;event},(fun x -> send x)

  let map_msg (f : 'a -> 'b) : 'a msg -> 'b msg = function
    | Set l -> Set (map_d f l)
    | Patch p -> Patch (map_p f p)

  let map f s =
    let current = ref (map_d f !(s.current)) in
    let event = React.E.map (fun msg ->
        let msg = map_msg f msg in
        begin match msg with
          | Set l -> current := l;
          | Patch p -> current := merge p !current
        end;
        msg) s.event in
    {current ;event}

  let value s = !(s.current)

  let event s = s.event

  let patch s p = s (Patch p)

  let set s p = s (Set p)

  let fold f s acc =
    let acc = f acc (Set (!(s.current))) in
    React.S.fold f acc s.event

  let value_s (s : 'a t) : 'a d React.S.t =
    React.S.fold (fun l msg ->
        match msg with
        | Set l -> l
        | Patch p -> merge p l) (!(s.current)) s.event

end

module DataList =   struct
  type 'a d = 'a list
  type 'a p =
    | I of int * 'a
    | R of int
    | A of 'a
    | C of 'a
    | X of int * int
    | U of int * 'a

  let map_d = List.map
  let map_p f = function
    | A x -> A (f x)
    | C x -> C (f x)
    | I (i,x) -> I (i, f x)
    | R i -> R i
    | X (i,j) -> X (i,j)
    | U (i,x) -> U (i,f x)

  let merge op l =
    match op with
    | C x -> x::l
    | A x -> l@[x]
    | I (i,x) ->
      let rec aux acc n l = match n,l with
        | 0,l -> List.rev_append acc (x::l)
        | _,[] -> assert false
        | n,x::xs -> aux (x::acc) (pred n) xs
      in aux [] i l
    | R i ->
      let rec aux acc n l = match n,l with
        | 0,x::l -> List.rev_append acc l
        | _,[] -> assert false
        | n,x::xs -> aux (x::acc) (pred n) xs
      in aux [] i l
    | X (i,j) ->
      let a = Array.of_list l in
      let tmp = a.(j) in
      a.(j) <- a.(i);
      a.(i) <- tmp;
      Array.to_list a
    | U (i,x) ->
      let a = Array.of_list l in
      a.(i) <- x;
      Array.to_list a
end

module RList = struct
  include Make (DataList)
  module D = DataList
  let append x s = patch s (D.A x)
  let cons x s = patch s (D.C x)
  let insert x i s = patch s (D.I (i,x))
  let update x i s = patch s (D.U (i,x))
  let swap i j s = patch s (D.X (i,j))
  let remove i s = patch s (D.R i)

  let concat : 'a t -> 'a t -> 'a t = fun x y ->
    let n,h = make [] in
    let e = React.E.select [React.E.map (fun s -> `E1 s) (event x);
                            React.E.map (fun s -> `E2 s) (event y)] in
    let size1 = ref 0
    and size2 = ref 0 in
    let update sizex = function
      | (D.I _ | D.C _ | D.A _ ) -> incr sizex
      | (D.R _) -> decr sizex
      | (D.X _ | D.U _) -> () in
    let f = function
      | `E1 (Patch p) ->
        let m = match p with
          | D.I _| D.C _| D.R _| D.U _ |D.X _ -> p
          | D.A x -> D.I (!size1,x)
        in
        update size1 m;
        patch h m
      | `E1 (Set l) ->
        size1 := List.length l;
        set h (l @ value y)
      | `E2 (Patch p) ->
        let m = match p with
          | D.A x -> p
          | D.I (pos,x) -> D.I (!size1 + pos, x)
          | D.R pos ->     D.R (pos + !size1)
          | D.U (pos,x) -> D.U (pos + !size1, x)
          | D.C x ->       D.I (!size1, x)
          | D.X (i,j) ->   D.X (i + !size1, j + !size1)
        in
        update size2 m;
        patch h m
      | `E2 (Set l) ->
        size2 := List.length l;
        set h (value x @ l) in
    let _ =
      f (`E1 (Set (value x)));
      f (`E2 (Set (value y)));
      React.E.map f e in
    n

end

module RMap(M : Map.S) = struct
  module Data = struct
    type 'a d = 'a M.t
    type 'a p = [`Add of (M.key * 'a) | `Del of M.key]
    let merge p s =
      match p with
      | `Add (k,a) -> M.add k a s
      | `Del k -> M.remove k s
    let map_p f = function
      | `Add (k,a) -> `Add (k,f a)
      | `Del k -> `Del k
    let map_d f d = M.map f d
  end
  include Make (Data)
end

module RArray = struct
  module Data = struct
    type 'a d = 'a array
    type 'a p = unit
    let merge p d = d
    let map_p f p = p
    let map_d f a = Array.map f a
  end
  include Make (Data)
end
