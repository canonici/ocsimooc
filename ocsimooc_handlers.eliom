
[%%server

 include Eba_handlers

 let upload_user_avatar_handler myid () ((), (cropping, photo)) =
   let avatar_dir =
     List.fold_left Filename.concat
       (List.hd !Ocsimooc_config.avatar_dir)
       (List.tl !Ocsimooc_config.avatar_dir) in
   let%lwt avatar =
     Eba_uploader.record_image avatar_dir ~ratio:1. ?cropping photo in
   let%lwt user = Eba_user.user_of_userid myid in
   let old_avatar = Eba_user.avatar_of_user user in
   let%lwt () = Eba_user.update_avatar avatar myid in
   match old_avatar with
   | None -> Lwt.return ()
   | Some old_avatar ->
     Lwt_unix.unlink (Filename.concat avatar_dir old_avatar )

 let forgot_password_handler =
   forgot_password_handler Eba_services.main_service

]

[%%client
     
 let set_personal_data_handler' =
    let set_personal_data_rpc =
      ~%(Eliom_client.server_function
	   [%derive.json : ((string * string) * (string * string))]
	   (Eba_session.connected_rpc
	      (fun id s -> set_personal_data_handler' id () s)))
    in
    fun (_ : int64) () d -> set_personal_data_rpc d

  let set_password_handler' id () p =
    Eba_handlers.set_password_rpc p

  let forgot_password_handler =
    let forgot_password_rpc =
      ~%(Eliom_client.server_function
	   [%derive.json : string]
	   (Eba_session.Opt.connected_rpc
	      (fun _ mail ->
		forgot_password_handler () mail)))
    in
    fun () mail -> forgot_password_rpc mail

  let preregister_handler' =
    let preregister_rpc =
      ~%(Eliom_client.server_function
	   [%derive.json : string]
	   (Eba_session.Opt.connected_rpc
	      (fun _ mail -> preregister_handler' () mail)))
    in
    fun () mail -> preregister_rpc mail
     
  let activation_handler =
    let activation_handler_rpc =
      ~%(Eliom_client.server_function
	   [%derive.json : string]
	   (Eba_session.Opt.connected_rpc
	      (fun _ akey -> activation_handler akey ())))
    in
    fun akey () -> activation_handler_rpc akey

]

let%shared password_form ~service () = Eliom_content.Html.D.(
  Form.post_form
    ~service
    (fun (pwdn, pwd2n) ->
       let pass1 =
         Form.input
           ~a:[a_required ();
               a_autocomplete false;
	       a_placeholder "password"]
           ~input_type:`Password
	   ~name:pwdn
           Form.string
       in
       let pass2 =
         Form.input
           ~a:[a_required ();
               a_autocomplete false;
	       a_placeholder "retype your password"]
           ~input_type:`Password
	   ~name:pwd2n
           Form.string
       in
       ignore [%client (
         let pass1 = Eliom_content.Html.To_dom.of_input ~%pass1 in
         let pass2 = Eliom_content.Html.To_dom.of_input ~%pass2 in
         Lwt_js_events.async
           (fun () ->
              Lwt_js_events.inputs pass2
                (fun _ _ ->
                   ignore (
		     if Js.to_string pass1##.value <> Js.to_string pass2##.value
                     then
		       (Js.Unsafe.coerce pass2)##(setCustomValidity ("Passwords do not match"))
                     else (Js.Unsafe.coerce pass2)##(setCustomValidity ("")));
                  Lwt.return ()))
	   : unit)];
       [
         table
           [
             tr [td [pass1]];
             tr [td [pass2]];
           ];
         Form.input ~input_type:`Submit
           ~a:[ a_class [ "button" ] ] ~value:"Send" Form.string
       ])
    ()
)

[%%shared
let about_handler userid_o () () = Eliom_content.Html.F.(
  Ocsimooc_container.page userid_o [
    div [
      p [pcdata "This template provides a skeleton \
                 for an Ocsigen application."];
      br ();
      p [pcdata "Feel free to modify the generated code and use it \
                 or redistribute it as you want."]
    ]
  ]
)

let settings_handler =
  let settings_content =
     let none = [%client ((fun () -> ()) : unit -> unit)] in
     fun user ->
       Eliom_content.Html.D.(
	 [
	   div ~a:[a_class ["eba-welcome-box"]] [
	     p [pcdata "Change your password:"];
	     password_form ~service:Eba_services.set_password_service' ();
	     br ();
	     Eba_userbox.upload_pic_link
	       none
	       Ocsimooc_services.upload_user_avatar_service
	       (Eba_user.userid_of_user user);
	     br ();
	     Eba_userbox.reset_tips_link none;
	   ]
	 ]
       )
   in
   fun userid_o () () ->
     let%lwt user = Ocsimooc_container.get_user_data userid_o in
     let content = match user with
       | Some user ->
	 settings_content user
       | None -> []
     in
     Ocsimooc_container.page userid_o content

]


let%server mooc_div :
    int64 option -> unit -> [`Div] Eliom_content.Html.elt Lwt.t
  = Eliom_content.Html.D.(
    let slide ~title ~content =
      title, Ocsimooc_lib.Content.ul content
    in
    let tst_slides n =
      let f i =
	Int64.of_int (20 * i),
	(let title = Printf.sprintf "Slide %d" i
	and content =
	   let open Ocsimooc_lib.Content in
	   !. "one"
	    |+ (!. "two.one"
		   |. "two.two")
	    |. "three"
	 in
	 slide ~title ~content)
      in
      Ocsimooc_lib.List.init f n
    in
    fun id_o () ->
      let e, get, set =
	Ocsimooc_play.show_with_video
	  "http://www.irill.org/media/OUPS/2014-06/Ocsigen.webm"
	  (tst_slides 50)
      in
      let%lwt input = Ocsimooc_msg.input get id_o in
      let%lwt messages = Ocsimooc_msg.display set id_o in
      Lwt.return @@ div ~a:[a_class ["ocsimooc-mooc"]] [e; messages; input]
  )

let%client mooc_div =
  let rpc = 
    ~%(Eliom_client.server_function [%derive.json : unit]
	 (Eba_session.Opt.connected_rpc mooc_div))
  in
  fun ( _ :int64 option) -> rpc

let%shared main_service_handler id_o () () =
  let%lwt d = mooc_div id_o () in
  Ocsimooc_container.page id_o [d]

