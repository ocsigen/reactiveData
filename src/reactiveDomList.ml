open ReactiveData
open RList

let insertAt dom i x =
  let nodes = dom##childNodes in
  assert (i <= nodes##length);
  if i = nodes##length
  then ignore(dom##appendChild((x :> Dom.node Js.t)))
  else ignore(dom##insertBefore(x,nodes##item(i)))

let merge_one_patch (dom : Dom.node Js.t) (p : Dom.node Js.t p) =
  match p with
  | I (i,x) ->
    let i = if i < 0 then dom##childNodes##length + 1 + i else i in
    insertAt dom i x
  | R i ->
    let i = if i < 0 then dom##childNodes##length + i else i in
    let nodes = dom##childNodes in
    assert (i < nodes##length);
    ignore(dom##removeChild(nodes##item(i)))
  | U (i,x) ->
    let i = if i < 0 then dom##childNodes##length + i else i in
    (match Js.Opt.to_option dom##childNodes##item(i) with
    | Some old -> ignore(dom##replaceChild(x,old))
    | _ -> assert false)
  | X (i,j) ->
    let i = if i < 0 then dom##childNodes##length + i else i in
    let j = if j < 0 then dom##childNodes##length + j else j in
    if i = j
    then ()
    else
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

let rec removeChildren dom =
  match Js.Opt.to_option dom##lastChild with
  | None -> ()
  | Some c ->
    ignore(dom##removeChild(c));
    removeChildren dom

let merge_msg (dom : Dom.node Js.t) (msg : Dom.node Js.t msg)  =
  match msg with
  | Set l ->
    (* Format.eprintf "replace all@."; *)
    removeChildren dom;
    List.iter (fun l -> ignore(dom##appendChild(l))) l;
  | Patch p ->
    (* Format.eprintf "patch@."; *)
    List.iter (merge_one_patch dom) p

let update_children (dom : Dom.node Js.t) (nodes : Dom.node Js.t t) =
  removeChildren dom;
  let _s : unit React.S.t = fold (fun () msg -> merge_msg dom msg) nodes ()
  in ()
