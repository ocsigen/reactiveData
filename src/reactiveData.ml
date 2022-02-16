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
  val diff : eq:('a -> 'a -> bool) -> 'a data -> 'a data -> 'a patch
end
module type S = sig
  type 'a t
  type 'a data
  type 'a patch
  type 'a msg = Patch of 'a patch | Set of 'a data
  type 'a handle
  val empty : 'a t
  val create : 'a data -> 'a t * 'a handle
  val from_event : 'a data -> 'a msg React.E.t -> 'a t
  val from_signal : ?eq:('a -> 'a -> bool) -> 'a data React.S.t -> 'a t
  val const : 'a data -> 'a t
  val patch : 'a handle -> 'a patch -> unit
  val set   : 'a handle -> 'a data -> unit
  val map_msg : ('a -> 'b) -> 'a msg -> 'b msg
  val map : ('a -> 'b) -> 'a t -> 'b t
  val value : 'a t -> 'a data
  val fold : ('a -> 'b msg -> 'a) -> 'b t -> 'a -> 'a React.signal
  val signal : ?eq:('a -> 'a -> bool) -> 'a t -> 'a data React.S.t
  val event : 'a t -> 'a msg React.E.t
end

module Make(D : DATA) :
  S with type 'a data = 'a D.data
     and type 'a patch = 'a D.patch = struct

  type 'a data = 'a D.data
  type 'a patch = 'a D.patch
  let merge = D.merge
  let map_patch = D.map_patch
  let map_data = D.map_data

  type 'a msg =
    | Patch of 'a patch
    | Set of 'a data

  type 'a handle = ?step:React.step -> 'a msg -> unit

  type 'a mut = {
    current : 'a data ref;
    event : 'a msg React.E.t;
  }

  type 'a t =
    | Const of 'a data
    | Mut of 'a mut

  let empty = Const D.empty

  let create l =
    let initial_event,send = React.E.create () in
    let current = ref l in
    let event = React.E.map (fun msg ->
        begin match msg with
          | Set l -> current := l;
          | Patch p -> current := merge p !current
        end;
        msg) initial_event in
    Mut {current;event},send

  let from_event l initial_event =
    let current = ref l in
    let event = React.E.map (fun msg ->
        begin match msg with
          | Set l -> current := l;
          | Patch p -> current := merge p !current
        end;
        msg) initial_event in
    Mut {current;event}

  let const x = Const x

  let map_msg (f : 'a -> 'b) : 'a msg -> 'b msg = function
    | Set l -> Set (map_data f l)
    | Patch p -> Patch (map_patch f p)

  let map f s =
    match s with
    | Const x -> Const (map_data f x)
    | Mut s ->
      let current = ref (map_data f !(s.current)) in
      let event = React.E.map (fun msg ->
          let msg = map_msg f msg in
          begin match msg with
            | Set l -> current := l;
            | Patch p -> current := merge p !current
          end;
          msg) s.event in
      Mut {current ;event}

  let value s = match s with
    | Const c -> c
    | Mut s -> !(s.current)

  let event s = match s with
    | Const _ -> React.E.never
    | Mut s -> s.event

  let patch (s : 'a handle) p = s (Patch p)

  let set (s : 'a handle) p = s (Set p)

  let fold f s acc =
    match s with
    | Const c -> React.S.const (f acc (Set c))
    | Mut s ->
      let acc = f acc (Set (!(s.current))) in
      React.S.fold f acc s.event

  let signal ?(eq = (=)) (s : 'a t) : 'a data React.S.t =
    match s with
    | Const c -> React.S.const c
    | Mut s ->
      React.S.fold ~eq:(D.equal eq) (fun l msg ->
          match msg with
          | Set l -> l
          | Patch p -> merge p l) (!(s.current)) s.event

  let from_signal ?(eq = (=)) s =
    let f d' d = Patch (D.diff ~eq d d') in
    from_event (React.S.value s) (React.S.diff f s)

end

module DataList = struct
  type 'a data = 'a list
  type 'a p =
    | I of int * 'a
    | R of int
    | U of int * 'a
    | X of int * int
  type 'a patch = 'a p list
  let empty = []
  let map_data = List.map
  let map_patch f = function
    | I (i,x) -> I (i, f x)
    | R i -> R i
    | X (i,j) -> X (i,j)
    | U (i,x) -> U (i,f x)
  let map_patch f = List.map (map_patch f)

  let merge_p op l =
    match op with
    | I (i',x) ->
      let i = if i' < 0 then List.length l + 1 + i' else i' in
      let rec aux acc n l = match n,l with
        | 0,l -> List.rev_append acc (x::l)
        | _,[] -> failwith "ReactiveData.Rlist.merge"
        | n,x::xs -> aux (x::acc) (pred n) xs
      in aux [] i l
    | R i' ->
      let i = if i' < 0 then List.length l + i' else i' in
      let rec aux acc n l = match n,l with
        | 0,_::l -> List.rev_append acc l
        | _,[] -> failwith "ReactiveData.Rlist.merge"
        | n,x::xs -> aux (x::acc) (pred n) xs
      in aux [] i l
    | U (i',x) ->
      let i = if i' < 0 then List.length l + i' else i' in
      let a = Array.of_list l in
      a.(i) <- x;
      Array.to_list a
    | X (i',offset) ->
      let a = Array.of_list l in
      let len = Array.length a in
      let i = if i' < 0 then len + i' else i' in
      let v = a.(i) in
      if offset > 0
      then begin
        if (i + offset >= len) then failwith "ReactiveData.Rlist.merge";
        for j = i to i + offset - 1 do
          a.(j) <- a.(j + 1)
        done;
        a.(i+offset) <- v
      end
      else begin
        if (i + offset < 0) then failwith "ReactiveData.Rlist.merge";
        for j = i downto i + offset + 1 do
          a.(j) <- a.(j - 1)
        done;
        a.(i+offset) <- v
      end;
      Array.to_list a

  (* accumulates into acc i unmodified elements from l *)
  let rec linear_merge_fwd ~acc i l =
    assert (i >= 0);
    if i > 0 then
      match l with
      | h :: l ->
        let acc = h :: acc in
        linear_merge_fwd ~acc (i - 1) l
      | [] ->
        invalid_arg "invalid index"
    else
      l, acc

  let rec linear_merge ~acc i0 p l =
    let l, acc =
      match p with
      | (I (i, _) | R i | U (i, _)) :: _ when i > i0 ->
        linear_merge_fwd ~acc (i - i0) l
      | _ ->
        l, acc
    in
    match p, l with
    | I (i, x) :: p, _ ->
      linear_merge ~acc i p (x :: l)
    | R i :: p, _ :: l ->
      linear_merge ~acc i p l
    | R _ :: _, [] ->
      invalid_arg "merge: invalid index"
    | U (i, x) :: p, _ :: l ->
      linear_merge ~acc i p (x :: l)
    | U (_, _) :: _, [] ->
      invalid_arg "merge: invalid index"
    | [], l ->
      List.rev_append acc l
    | X (_, _) :: _, _ ->
      failwith "linear_merge: X not supported"

  let rec linear_mergeable ~n p =
    assert (n >= 0);
    match p with
    | (I (i, _) | R i | U (i, _)) :: p when i >= n ->
      (* negative i's ruled out (among others) *)
      linear_mergeable ~n:i p
    | _ :: _ ->
      false
    | [] ->
      true

  let merge p l =
    if linear_mergeable ~n:0 p then
      linear_merge ~acc:[] 0 p l
    else
      List.fold_left (fun l x -> merge_p x l) l p

  let rec equal f l1 l2 =
    match l1, l2 with
    | x1 :: l1, x2 :: l2 when f x1 x2 ->
      equal f l1 l2
    | [], [] ->
      true
    | _ :: _ , _ :: _
    | _ :: _ , []
    | []     , _ :: _ ->
      false

  let mem (type u) l =
    let module H =
      Hashtbl.Make
        (struct type t = u let hash = Hashtbl.hash let equal = (==) end) in
    let h = H.create 16 in
    List.iter (fun x -> H.add h x ()) l;
    H.mem h

  let fold_diff ?(eq = (=)) ~acc ~remove ~add lx ly =
    let memx = mem lx
    and memy = mem ly in
    let rec f ~acc ~left lx ly n =
      match lx, ly with
      (* trailing elements to be removed *)
      | _ :: lx, [] ->
        let acc = remove acc n in
        f ~acc ~left lx [] n
      (* trailing elements to be added *)
      | [], y :: ly ->
        let acc = add acc n y in
        f ~acc ~left [] ly (n + 1)
      (* done! *)
      | [], [] ->
        acc
      (* same *)
      | x :: lx, y :: ly when eq x y ->
        f ~acc ~left lx ly (n + 1)
      (* x needs to be removed for sure *)
      | x :: lx, _ :: _ when not (memy x) ->
        let acc = remove acc n in
        f ~acc ~left lx ly n
      (* y needs to be added for sure *)
      | _ :: _, y :: ly when not (memx y) ->
        let acc = add acc n y in
        f ~acc ~left lx ly (n + 1)
      (* no more certainty, ~left decides what to recur on *)
      | _ :: lx, _ :: _ when left ->
        let acc = remove acc n in
        f ~acc ~left:false lx ly n
      | _ :: _, y :: ly ->
        let acc = add acc n y in
        f ~acc ~left:true lx ly (n + 1)
    in
    f ~acc ~left:true lx ly 0

  let rec list_rev ?(acc = []) = function
    | h :: t ->
      let acc = h :: acc in
      list_rev ~acc t
    | [] ->
      acc

  let diff ~eq x y =
    let add acc i v = I (i, v) :: acc
    and remove acc i = R i :: acc
    and acc = [] in
    list_rev (fold_diff ~eq ~acc ~add ~remove x y)

end

module RList = struct
  include Make (DataList)
  module D = DataList
  type 'a p = 'a D.p =
    | I of int * 'a
    | R of int
    | U of int * 'a
    | X of int * int

  let cons x s = patch s [D.I (0,x)]
  let snoc x s = patch s [D.I (-1,x)]
  let insert x i s = patch s [D.I (i,x)]
  let update x i s = patch s [D.U (i,x)]
  let move i j s = patch s [D.X (i,j)]
  let remove i s = patch s [D.R i]

  let index ?(eq = (=)) l x =
    let rec f n = function
      | hd :: _ when eq hd x -> n
      | _ :: tl -> f (n + 1) tl
      | [] -> raise Not_found
    in
    f 0 l

  let update_eq ?eq (data, handle) x y =
    let i = index ?eq (value data) x in
    update y i handle

  let remove_last (data, handle) =
    remove (List.length (value data) - 1) handle

  let remove_eq ?eq (data, handle) x =
    let i = index ?eq (value data) x in
    remove i handle

  let singleton x = const [x]

  let singleton_s s =
    let first = ref true in
    let e,send = React.E.create () in
    let result = from_event [] e in
    let _ = React.S.map (fun x ->
        if !first
        then begin
          first:=false;
          send (Patch [I(0,x)])
        end
        else send (Patch [U(0,x)])) s in
    result

  let concat : 'a t -> 'a t -> 'a t = fun x y ->
    let v1 = value x
    and v2 = value y in
    let size1 = ref 0
    and size2 = ref 0 in
    let size_with_patch sizex : 'a D.p -> unit = function
      | (D.I _) -> incr sizex
      | (D.R _) -> decr sizex
      | (D.X _ | D.U _) -> () in
    let size_with_set sizex l = sizex:=List.length l in

    size_with_set size1 v1;
    size_with_set size2 v2;

    let update_patch1 = List.map (fun p ->
        let m = match p with
          | D.I (pos,x) ->
            let i = if pos < 0 then pos - !size2 else pos in
            D.I (i, x)
          | D.R pos     -> D.R  (if pos < 0 then pos - !size2 else pos)
          | D.U (pos,x) -> D.U ((if pos < 0 then pos - !size2 else pos), x)
          | D.X (i,j) ->   D.X ((if i < 0 then i - !size2 else i),j)
        in
        size_with_patch size1 m;
        m) in
    let update_patch2 = List.map (fun p ->
        let m = match p with
          | D.I (pos,x) -> D.I ((if pos < 0 then pos else !size1 + pos), x)
          | D.R pos     -> D.R  (if pos < 0 then pos else !size1 + pos)
          | D.U (pos,x) -> D.U ((if pos < 0 then pos else !size1 + pos), x)
          | D.X (i,j) ->   D.X ((if i < 0 then i else !size1 + i),j)
        in
        size_with_patch size2 m;
        m) in
    let tuple_ev =
      React.E.merge (fun acc x ->
          match acc,x with
          | (None,p2),`E1 x -> Some x,p2
          | (p1,None),`E2 x -> p1, Some x
          | _ -> assert false)
        (None,None)
        [React.E.map (fun e -> `E1 e) (event x);
         React.E.map (fun e -> `E2 e) (event y)] in
    let merged_ev = React.E.map (fun p ->
        match p with
        | Some (Set p1), Some (Set p2) ->
          size_with_set size1 p1;
          size_with_set size2 p2;
          Set (p1 @ p2)
        | Some (Set p1), None ->
          size_with_set size1 p1;
          Set (p1 @ value y)
        | None, Some (Set p2) ->
          size_with_set size2 p2;
          Set (value x @ p2 )
        | Some (Patch p1), Some (Patch p2) ->
          let p1 = update_patch1 p1 in
          let p2 = update_patch2 p2 in
          Patch (p1 @ p2)
        | Some (Patch p1), None -> Patch (update_patch1 p1)
        | None, Some (Patch p2) -> Patch (update_patch2 p2)
        | Some (Patch _), Some (Set s2) ->
          let s1 = value x in
          size_with_set size1 s1;
          size_with_set size2 s2;
          Set(s1 @ s2)
        | Some (Set s1), Some (Patch _) ->
          size_with_set size1 s1;
          let s2 = value y in
          size_with_set size2 s2;
          Set(s1 @ s2)
        | None,None -> assert false
      ) tuple_ev in
    from_event (v1 @ v2) merged_ev

  let inverse : 'a . 'a p -> 'a p = function
    | I (i,x) -> I(-i-1, x)
    | U (i,x) -> U(-i-1, x)
    | R i -> R (-i-1)
    | X (i,j) -> X (-i-1,-j)

  let rev t =
    let e = React.E.map (function
        | Set l -> Set (List.rev l)
        | Patch p -> Patch (List.map inverse p))  (event t)
    in
    from_event (List.rev (value t)) e

  let filter pred l =
    let module IntMap = Map.Make(Int) in

    let index = ref IntMap.empty in
    let size = ref 0 in

    let filter_list l =
      let rec aux (l: 'a list) res their_i my_i = match l with
        | [] -> res
        | x::xs ->
           if pred x
           then
             begin
               index := IntMap.add their_i (my_i + 1) !index;
               aux xs (x::res) (their_i + 1) (my_i + 1)
             end
           else
             begin
               aux xs res (their_i + 1) my_i
             end
      in
      size := List.length l;
      index := IntMap.empty;
      List.rev (aux l [] 0 (-1))
    in

    let normalise i = if i < 0 then !size + 1 + i else i in

    let update_index_insert insert_pos_full_list visible =
      let insert_pos_full_list = normalise insert_pos_full_list in
      let left_alone, displaced, updatables = IntMap.split insert_pos_full_list !index in
      let updatables = match displaced with
        | None -> updatables
        | Some displaced_in_filtered ->
           IntMap.add insert_pos_full_list displaced_in_filtered updatables
      in
      let update_j j_full_list j_filtered_list =
        let new_j_filtered = if visible then j_filtered_list + 1 else j_filtered_list in
        index := IntMap.add (j_full_list + 1) new_j_filtered !index
      in
      let () = IntMap.iter update_j updatables in
      let insert_pos_filtered = if IntMap.is_empty left_alone
                                then 0
                                else (snd (IntMap.max_binding left_alone)) + 1
      in
      if visible then index := IntMap.add insert_pos_full_list insert_pos_filtered !index;
      incr size;
      insert_pos_filtered
    in

    let update_index_remove remove_pos_full_list =
      let was_visible = IntMap.mem remove_pos_full_list !index in
      let _,_,updatables = IntMap.split remove_pos_full_list !index in
      let update_j j_full_list j_filtered_list =
        let new_j = if was_visible then j_filtered_list else j_filtered_list - 1 in
        index := IntMap.add (j_full_list - 1) new_j !index
      in
      if not (IntMap.is_empty !index)
      then
        let last_i, _ = IntMap.max_binding !index in
        index := IntMap.remove last_i !index
      else ();
      decr size;
      IntMap.iter update_j updatables
    in

    let update_index_update_delete update_pos_full_list =
      let _, _, updatables = IntMap.split update_pos_full_list !index in
      let update_j j_full_list j_filtered_list =
        index := IntMap.add j_full_list (j_filtered_list - 1) !index
      in
      index := IntMap.remove update_pos_full_list !index;
      IntMap.iter update_j updatables
    in

    let update_index_update_insert update_pos_full_list =
      let left_alone, none, updatables = IntMap.split update_pos_full_list !index in
      assert (none = None);
      let update_j j_full_list j_filtered_list =
        index := IntMap.add j_full_list (j_filtered_list + 1) !index
      in
      let new_pos_filtered_list =
        let previous_pos_filtered = try (snd (IntMap.max_binding left_alone)) with Not_found -> (-1) in
        previous_pos_filtered + 1
      in
      index := IntMap.add update_pos_full_list new_pos_filtered_list !index;
      IntMap.iter update_j updatables;
      new_pos_filtered_list
    in

    let update_index_move from_full_list to_full_list to_filtered =
      let was_visible = match to_filtered with | Some _ -> true | None -> false in
      let forward = from_full_list < to_full_list in
      if forward then
        for i_full = from_full_list + 1 to to_full_list do
          let delta = if was_visible then (-1) else 0 in
          try
            let i_filtered = IntMap.find i_full !index in
            let new_val = i_filtered + delta in
            index := IntMap.add (i_full - 1) new_val !index
          with
          | Not_found -> ()
        done
      else
        for i_full = from_full_list - 1 downto to_full_list do
          try
            let delta = if was_visible then 1 else 0 in
            let i_filtered = IntMap.find i_full !index in
            let new_val = i_filtered + delta in
            index := IntMap.add (i_full + 1) new_val !index
          with
          | Not_found -> ()
        done;
      match to_filtered with
      | Some to_filtered ->
         index := IntMap.add to_full_list to_filtered !index
      | None ->
         index := IntMap.remove to_full_list !index
    in

    let convert_p = function
      | I (i, x) ->
         if pred x
         then
           let my_i = update_index_insert i true in
           [I (my_i, x)]
         else
           begin
             ignore (update_index_insert i false);
             []
           end
      | R i ->
         let i = normalise i in
         let ret = try let j = IntMap.find i !index in [R j] with | Not_found -> [] in
         let () = update_index_remove i in
         ret
      | U (i, x) ->
         let i = normalise i in
         begin
           try
             let old_j = IntMap.find i !index in
             if pred x
             then [U (old_j, x)]
             else (update_index_update_delete i; [R old_j])
           with
           | Not_found ->
              if pred x
              then
                let new_j = update_index_update_insert i in
                [I (new_j, x)]
              else
                []
         end
      | X (origin_full, offset_full) ->
         let origin_full = normalise origin_full in
         let dest_full = origin_full + offset_full in
         try
           let origin_filtered = IntMap.find origin_full !index in
           let dest_filtered =
             try IntMap.find dest_full !index
             with
             | Not_found ->
                let small_ones, _, _ = IntMap.split origin_full !index in
                if IntMap.is_empty small_ones
                then 0
                else snd (IntMap.max_binding small_ones) + 1
           in
           update_index_move origin_full dest_full (Some dest_filtered);
           if dest_filtered != origin_filtered
           then [X (origin_filtered, dest_filtered - origin_filtered)]
           else []
         with
         | Not_found ->
            (* moving an element that was filtered out *)
            update_index_move origin_full dest_full None;
            []
    in

    let filter_e = function
      | Set l -> Set (filter_list l)
      | Patch p ->
         Patch (List.concat (List.map convert_p p))
    in
    let e = React.E.map filter_e (event l) in
    from_event (filter_list (value l)) e

  module IntSet = Set.Make (Int)

  let for_all fn data =
    let maybe_update acc i v = if fn v then acc else IntSet.add i acc in
    let init =
      let rec fold i acc = function
        | v :: tl -> fold (i + 1) (maybe_update acc i v) tl
        | [] -> acc
      in
      fold 0 IntSet.empty
    in
    let update_idx_after i f acc =
      IntSet.map (fun i' -> if i' >= i then f i' 1 else i') acc
    in
    let f = fun acc -> function
      | Set x -> init x
      | Patch updates ->
        List.fold_left
          (fun acc -> function
             | X (i, i') ->
               if IntSet.mem i acc = IntSet.mem i' acc
               then acc
               else if IntSet.mem i acc
               then IntSet.add i' (IntSet.remove i acc)
               else IntSet.add i (IntSet.remove i' acc)
             | R i ->
               update_idx_after i (-) (IntSet.remove i acc)
             | I (i, v) ->
               let acc = update_idx_after i (+) acc in
               maybe_update acc i v
             | U (i, v) ->
               maybe_update (IntSet.remove i acc) i v)
          acc
          updates
    in
    React.S.fold f (init (value data)) (event data)
    |> React.S.map IntSet.is_empty

end

module RMap(M : Map.S) = struct

  module Data = struct

    type 'a data = 'a M.t

    type 'a p = [`Add of (M.key * 'a) | `Del of M.key]

    type 'a patch = 'a p list

    let merge_p p s =
      match p with
      | `Add (k,a) -> M.add k a s
      | `Del k -> M.remove k s

    let merge p acc = List.fold_left (fun acc p -> merge_p p acc) acc p

    let map_p f = function
      | `Add (k,a) -> `Add (k,f a)
      | `Del k -> `Del k

    let map_patch f = List.map (map_p f)

    let map_data f d = M.map f d

    let empty = M.empty

    let equal f = M.equal f

    let diff ~eq x y =
      let m =
        let g _key v w =
          match v, w with
          | Some v, Some w when eq v w ->
            None
          | Some _, Some w ->
            Some (`U w)
          | Some _, None ->
            Some `D
          | None, Some v ->
            Some (`A v)
          | None, None ->
            None
        in
        M.merge g x y
      and g key x acc =
        match x with
        | `U v ->
          `Del key :: `Add (key, v) :: acc
        | `D ->
          `Del key :: acc
        | `A v ->
          `Add (key, v) :: acc
      and acc = [] in
      List.rev (M.fold g m acc)

  end

  include Make(Data)

  let filter pred m =

    let convert_p = function
      | `Add (k,v) ->
         if pred k v
         then [`Add (k,v)]
         else []
      | `Del k -> [`Del k]
    in

    let filter_e = function
      | Set m -> Set (M.filter pred m)
      | Patch p -> Patch (List.concat (List.map convert_p p))
    in

    let e = React.E.map filter_e (event m) in
    from_event (M.filter pred (value m)) e
end
