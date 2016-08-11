[%%shared
 open Ocsimooc_lib
 open Eliom_shared.React.S.Infix
]

module Eliom_lib = struct
  include Eliom_lib
  let alert = Printf.printf
end ;;

let%shared video_elt v = Eliom_content.Html.D.(
  let src = Eliom_content.Xml.uri_of_string v
  and srcs = []
  and a = [a_controls ()] in
  video ~a ~src ~srcs []
)

let%shared array_of_slides l =
  let a = Array.of_list l in
  let n = Array.length a in
  assert
    (let f i (m, _) =
       i >= n - 1 || (let m', _ = a.(i + 1) in m <= m')
     in
     Array.for_alli f a);
  a

[%%client

 module BST = BSTree.Make (struct
   type key = int64 * int64
   type value = int
   let compare (a,b) (c,d) =
     if a <= c && c < b && a <= d && d < b then 0
     else if b <= c then -1
     else 1
 end)

let handle_timeupdate v f =
  Lwt.async (fun () ->
    let f ev u = Lwt.return (f (v##.currentTime)) in
    Lwt_js_events.timeupdates v f)

let handle_timeupdate_slides a v f =
(*  let last, _ = a.(Array.length a - 1) in*)
  let _,_,bst = Array.fold_right
    (fun (i,_) (n,pi,t) -> pred n, i, BST.insert t ((i,pi),n))
    a
    (Array.length a - 1, Int64.max_int, BST.empty)
  in
  let g m =
    let m = Int64.of_float m in
    BST.search bst (m,m) |> fun io ->
      let i = match io with Some i -> i | None -> 0 in
      f (false, i)
  in
  (*let g m =
    (let f (m', _) = m' > Int64.of_float m in
     Array.find f a) |> Opt.iter (fun i -> f (false, if i = 0 then 0 else i-1))
  in*)
  handle_timeupdate v g
]

[%%shared

let slide_body a (_,i) =
  let _, (_, bd) = a.(i) in bd

let slide_title a (_,i) =
  let _, (title, _) = a.(i) in title

let next (a : _ array) r f () =
  let _,v = Eliom_shared.(React.S.value r |> Value.local) in
  (f : (bool * int) -> unit) (true, if v >= Array.length a - 1 then 0 else v + 1)

let prev (a : _ array) r f () =
  let _,v = Eliom_shared.(React.S.value r |> Value.local) in
  (f : (bool * int) -> unit) (true, if v <= 0 then Array.length a - 1 else v - 1)
]

let%shared prev_button (y : _ array) (r, upd) =Eliom_content.Html.D.(
  let a =
    [
      a_class ["prev-button"];
      a_onclick [%client (fun _ -> prev ~%y ~%r ~%upd ())]
    ]
  in
  span ~a [pcdata "prev"]
)

let%shared next_button y (r, upd) = Eliom_content.Html.D.(
  let a =
    [
      a_class ["next-button"];
      a_onclick [%client (fun _ -> next ~%y ~%r ~%upd ())]
    ]
  in
  span ~a [pcdata "next"]
)

let%shared slide_container y ((r, upd) as rr) (b_r, b_upd) =
  let open Eliom_content.Html.F in
  let ttl =
    let a =
      [
	a_class ["slide-title"];
	a_onclick [%client (fun _ -> ~%b_r |> React.S.value |> not |> ~%b_upd)]
      ]
    in
    span ~a
      [r >|= [%shared slide_title ~%y] |>
       Eliom_content.Html.R.pcdata]
  and bd =
    let s = r >|= [%shared slide_body ~%y] in
    div ~a:[a_class ["slide-body"]] [Eliom_content.Html.R.node s]
  in
  [prev_button y rr; ttl; next_button y rr; bd] |>
  div ~a:[a_class ["slide-container"]]

let%shared clickable_list v l (b_r, b_upd) = Eliom_content.Html.D.(
  let l =
    let action m = [%client
      let v = Eliom_content.Html.To_dom.of_video ~%v in
      fun _ ->
        v##.currentTime := Js.float @@ Int64.to_float ~%m;
        ~%b_upd true]
    in
    let f (m, (s, _)) =
      li ~a:[a_onclick (action m)] [pcdata s]
    in
    List.map f l
  and a =
    [a_class ["slide-list"];
     Eliom_content.Html.R.filter_attrib (a_class ["hidden"]) b_r;
     Eliom_content.Html.R.filter_attrib (a_hidden ()) b_r]
  in
  ul ~a l
)

let%shared show_with_video v l = Eliom_content.Html.D.(
  let a = array_of_slides l in
  let (r, upd) as rr = Eliom_shared.React.S.create (false, 0)
  and (b_r, b_upd) as brr = Eliom_shared.React.S.create true
  and v = video_elt v in
  let v' = div ~a:[a_class ["video-container"]] [v]
  and _ = ignore [%client (
    let v = Eliom_content.Html.To_dom.of_video ~%v in
    handle_timeupdate_slides ~%a v ~%upd;
    React.E.map
      (fun (b,i) ->
	if b
	then
          let m, _ = (~%a).(i) in
          v##.currentTime := Int64.to_float m
	else ()
      )
      (React.S.changes ~%r) |> ignore
  : unit)]
  and get = [%client (
    let v = Eliom_content.Html.To_dom.of_video ~%v in
    fun () -> v##.currentTime
  : unit -> float)]
  and set = [%client (
    let v = Eliom_content.Html.To_dom.of_video ~%v in
    fun x -> v##.currentTime := x
  : float -> unit)] in
  (*let debug = Eliom_shared.React.S.map [%shared string_of_int] r in
  let rdebug = Eliom_content.Html.R.pcdata debug in*)
  (*ignore [%client (
    let v = Eliom_content.Html.To_dom.of_video ~%v in
    v##.preload := Js.string "auto"
  : unit)];*)
  div [v'; slide_container a rr brr; clickable_list v l brr(*; p [rdebug]*)],
  get, set
)
