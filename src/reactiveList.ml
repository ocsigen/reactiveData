module Patch = struct
  type 'a t =
    | I of int * 'a
    | R of int
    | A of 'a
    | C of 'a
    | X of int * int
    | U of int * 'a


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

module P = Patch

type 'a msg =
  | Patch of 'a P.t
  | Set of 'a list

type 'a handle = 'a msg -> unit

type 'a t = {
  current : 'a list ref;
  event : 'a msg React.E.t;
}

let make l =
  let initial_event,send = React.E.create () in
  let current = ref l in
  let event = React.E.map (fun msg ->
      begin match msg with
        | Set l -> current := l;
        | Patch p -> current := P.merge p !current
      end;
      msg) initial_event in
  {current;event},(fun x -> send x)

let map_msg (f : 'a -> 'b) : 'a msg -> 'b msg = function
  | Set l -> Set (List.map f l)
  | Patch (P.A x) -> Patch (P.A (f x))
  | Patch (P.C x) -> Patch (P.C (f x))
  | Patch (P.I (i,x)) -> Patch (P.I (i, f x))
  | Patch (P.R i) -> Patch (P.R i)
  | Patch (P.X (i,j)) -> Patch (P.X (i,j))
  | Patch (P.U (i,x)) -> Patch (P.U (i,f x))

let map f s =
  let current = ref (List.map f !(s.current)) in
  let event = React.E.map (fun msg ->
      let msg = map_msg f msg in
      begin match msg with
        | Set l -> current := l;
        | Patch p -> current := P.merge p !current
      end;
      msg) s.event in
  {current ;event}
let set l s = s (Set l)
let append x s = s (Patch (P.A x))
let cons x s = s (Patch (P.C x))
let insert x i s = s (Patch (P.I (i,x)))
let update x i s = s (Patch (P.U (i,x)))
let swap i j s = s (Patch (P.X (i,j)))

let sort f s = assert false

let remove i s = s (Patch (P.R i))
let value s = !(s.current)
let fold f s acc =
  let acc = f acc (Set (!(s.current))) in
  React.S.fold f acc s.event
let list (s : 'a t) : 'a list React.S.t =
  React.S.fold (fun l msg ->
      match msg with
      | Set l -> l
      | Patch p -> P.merge p l) (!(s.current)) s.event
