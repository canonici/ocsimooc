
let%server display f userid_o =
  Ocsimooc_msg_db.Notif.listen ();
  let%lwt l = Ocsimooc_msg_db.get_indices () in
  let%lwt l =
    let%lwt e =
      let%lwt ev = Ocsimooc_msg_db.Notif.client_ev () in
      Lwt.return [%client React.E.map snd ~%ev]
    in
    let init = Eliom_shared.ReactiveData.RList.create l in
    Lwt.return @@ Eliom_shared.ReactiveData.RList.acc_e ~init e
  in
  let f =
    [%shared
       fun id ->
         let f = ~%f in
         let%lwt (username, m, s) = Ocsimooc_msg_db.get id in
         let timestamp =
           let open Eliom_content.Html.D in
           let a =
             [a_onclick [%client (fun _ -> ~%f ~%m)];
              a_class ["msg-time"]]
           and l = [pcdata (string_of_int @@ int_of_float m)] in
           span ~a l
         and msg =
           let open Eliom_content.Html.D in
           let a = [a_class ["msg-content"]]
           and l = [pcdata s] in
           span ~a l
	 and username =
           let open Eliom_content.Html.D in
           let a = [a_class ["msg-from"]]
           and l = [pcdata username] in
           span ~a l
         in
         Lwt.return (Eliom_content.Html.D.(
	   li [timestamp; username; msg]
	 ))]
  in
  let%lwt l = Eliom_shared.ReactiveData.RList.Lwt.map_p f l in
  let a = [Eliom_content.Html.D.a_class ["msg-list"]] in
  Lwt.return (Eliom_content.Html.R.ul ~a l) ;;

let%shared input f = Eliom_content.Html.D.(function
  | None ->
    Lwt.return @@ div [pcdata "You must be connected to comment."]
  | Some id ->
    let inp = Raw.input ~a:[a_input_type `Text] () in
    let _ = [%client (
      let open Lwt_js_events in
      let inp = Eliom_content.Html.To_dom.of_input ~%inp in
      async (fun () -> changes inp (fun _ _ ->
	let value = Js.to_string (inp##.value) in
	inp##.value := Js.string "";
	let%lwt _ = Ocsimooc_msg_db.add (~%f ()) value in
	Lwt.return ()))
	: unit)] in
    Lwt.return @@ div [inp])
