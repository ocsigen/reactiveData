open ReactiveList

module P = Patch
let insertAt dom i x =
  let nodes = dom##childNodes in
  assert (i <= nodes##length);
  if i = nodes##length
  then ignore(dom##appendChild((x :> Dom.node Js.t)))
  else ignore(dom##insertBefore(x,nodes##item(i)))

let merge_patch (dom : Dom.node Js.t) (p : Dom.node Js.t P.t) =
  match p with
  | P.C x ->
    begin match Js.Opt.to_option dom##firstChild with
      | Some _ -> ignore(dom##insertBefore(x,dom##firstChild))
      | None -> ignore(dom##appendChild ((x :> Dom.node Js.t)))
    end
  | P.A x -> ignore(dom##appendChild ((x :> Dom.node Js.t)))
  | P.I (i,x) -> insertAt dom i x
  | P.R i ->
    let nodes = dom##childNodes in
    assert (i < nodes##length);
    ignore(dom##removeChild(nodes##item(i)))
  | P.X (i,j) when i = j -> ()
  | P.X (i,j) ->
    let i, j = if i > j then j,i else i,j in
    begin
      match Js.Opt.to_option dom##childNodes##item(i),
            Js.Opt.to_option dom##childNodes##item(j)
      with
      | Some i', Some j' ->
        insertAt dom j i';
        insertAt dom i j'

      | _ -> assert false
    end
  | P.U (i,x) ->
    match Js.Opt.to_option dom##childNodes##item(i) with
    | Some old -> ignore(dom##replaceChild(x,old))
    | _ -> assert false

let rec removeChildren dom =
  match Js.Opt.to_option dom##lastChild with
  | None -> ()
  | Some c ->
    ignore(dom##removeChild(c));
    removeChildren dom

let merge_msg (dom : Dom.node Js.t) (msg : Dom.node Js.t ReactiveList.msg)  =
  match msg with
  | Set l ->
    (* Format.eprintf "replace all@."; *)
    removeChildren dom;
    List.iter (fun l -> ignore(dom##appendChild(l))) l;
  | Patch p ->
    (* Format.eprintf "patch@."; *)
    merge_patch dom p

let update_children (dom : Dom.node Js.t) (nodes : Dom.node Js.t ReactiveList.t) =
  removeChildren dom;
  let _s : unit React.S.t = ReactiveList.fold (fun () msg -> merge_msg dom msg) nodes ()
  in ()
