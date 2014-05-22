open ReactiveData
open RList


let _ = Lwt_js.yield
(* create a reactive list of int *)
let all,handle = make [0;1;2]

(* identity *)
let ints = map (fun x -> x) all

(* convert to string *)
let strings : string RList.t = map string_of_int ints

let size : int React.E.t = React.S.changes (React.S.map List.length (RList.value_s strings))
let size2 = make_from [] (React.E.map (fun v -> Set [v]) size)
let size3 = map string_of_int size2
let size4 = map Js.string size3

(* convert to javascript string *)
let js_strings1 = map Js.string strings
let js_strings2, handle2 = make [Js.string "one";Js.string "two"]

let js_strings'' = concat js_strings2 (rev js_strings1)
let js_strings' = concat js_strings1 js_strings''
let js_strings = concat size4 js_strings'

let make_span t =
  let t = (Dom_html.document##createTextNode(t) :> Dom.node Js.t) in
  let s = Dom_html.createSpan Dom_html.document in
  s##style##width <- Js.string "50px;";
  s##style##display <- Js.string "block";
  ignore(s##appendChild(t));
  (s :> Dom.node Js.t)

(* dom span element *)
let spans : Dom.node Js.t RList.t = map make_span js_strings

let id x =
  let content = Dom_html.document##getElementById(Js.string x) in
  Js.Opt.get content (fun _ -> assert false)


let _ = Dom_html.window##onload <- Dom.handler (fun _ ->
    let content = id "main" in
    ReactiveDomList.update_children (content :> Dom.node Js.t) spans;

    let item,hand = make [] in
    let input : Dom_html.inputElement Js.t = Js.Unsafe.coerce (id "input") in
    let msgs = id "msgs" in
    ReactiveDomList.update_children (msgs :> Dom.node Js.t) (map (fun s -> make_span (Js.string s)) item);
    input##onchange <- Dom_html.handler (fun _ ->
        let s = input##value in
        input##value <- Js.string "";
        append (Js.to_string s) hand;
        Js._true);

    Js._true)

let _ =
    Js.Unsafe.global##cons <- (fun s -> cons s handle);
    Js.Unsafe.global##remove <- (fun s -> remove s handle);
    Js.Unsafe.global##append <- (fun s -> append s handle);
    Js.Unsafe.global##insert <- (fun s i -> insert s i handle);
    Js.Unsafe.global##swap <- (fun i j -> swap i j handle);
    Js.Unsafe.global##update <- (fun s i -> update s i handle);
    Js.Unsafe.global##cons2 <- (fun s -> cons s handle2);
    Js.Unsafe.global##remove2 <- (fun s -> remove s handle2);
    Js.Unsafe.global##append2 <- (fun s -> append s handle2);
    Js.Unsafe.global##insert2 <- (fun s i -> insert s i handle2);
    Js.Unsafe.global##swap2 <- (fun i j -> swap i j handle2);
    Js.Unsafe.global##update2 <- (fun s i -> update s i handle2);

    Js.Unsafe.global##shuffle <- (fun () ->
        let len = List.length (value all) in
        let rec shuf () =
          let i = Random.int len in
          let j = Random.int len in
          if i <> j
          then
            swap i j handle
          else shuf () in
        shuf ()
      );

    Js.Unsafe.global##shuffle2 <- (fun () ->
        let len = List.length (value js_strings2) in
        let rec shuf () =
          let i = Random.int len in
          let j = Random.int len in
          if i <> j
          then
            swap i j handle2
          else shuf () in
        shuf ()
      );
    Js.Unsafe.global##show <- (fun () ->
        let s = String.concat ", " (List.map Js.to_string  (value js_strings)) in
        Js.string s
      )




(* let rl,lh = make [5;8;10] *)
(* let sl = map string_of_int rl *)
(* let eff = map print_endline sl *)


(* let _ = fold ;; *)

(* let () = cons 50 lh *)
(* let _ = value sl;; *)
