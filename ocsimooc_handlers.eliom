
[%%server

 include Os_handlers

 let upload_user_avatar_handler myid () ((), (cropping, photo)) =
   let avatar_dir =
     List.fold_left Filename.concat
       (List.hd !Ocsimooc_config.avatar_dir)
       (List.tl !Ocsimooc_config.avatar_dir) in
   let%lwt avatar =
     Os_uploader.record_image avatar_dir ~ratio:1. ?cropping photo in
   let%lwt user = Os_user.user_of_userid myid in
   let old_avatar = Os_user.avatar_of_user user in
   let%lwt () = Os_user.update_avatar avatar myid in
   match old_avatar with
   | None -> Lwt.return ()
   | Some old_avatar ->
     Lwt_unix.unlink (Filename.concat avatar_dir old_avatar )

 let forgot_password_handler =
   forgot_password_handler Os_services.main_service

 let set_personal_data_handler' =
   Os_session.connected_fun set_personal_data_handler'

 let set_password_handler' =
   Os_session.connected_fun set_password_handler'

]

[%%client

 let set_personal_data_handler' =
   let set_personal_data_rpc =
     ~%(Eliom_client.server_function
	  [%derive.json : ((string * string) * (string * string))]
	@@ set_personal_data_handler' ())
   in
   fun () -> set_personal_data_rpc

 let set_password_handler' () = Os_handlers.set_password_rpc

 let forgot_password_handler =
   let forgot_password_rpc =
     ~%(Eliom_client.server_function [%derive.json : string]
	@@ forgot_password_handler ())
   in
   fun () -> forgot_password_rpc

  let preregister_handler' =
    let preregister_rpc =
      ~%(Eliom_client.server_function [%derive.json : string]
	 @@ preregister_handler' ())
    in
    fun () -> preregister_rpc
     
  let activation_handler =
    let activation_handler_rpc =
      ~%(Eliom_client.server_function [%derive.json : string]
	 @@ fun akey -> activation_handler akey ())
    in
    fun akey () -> activation_handler_rpc akey

]


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

 let settings_handler userid_o () () =
   let%lwt user = Ocsimooc_container.get_user_data userid_o in
   let content = match user with
     | Some user ->
       Ocsimooc_content.Settings.settings_content user
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
	 (Os_session.Opt.connected_rpc mooc_div))
  in
  fun ( _ :int64 option) -> rpc

let%shared main_service_handler id_o () () =
  let%lwt d = mooc_div id_o () in
  Ocsimooc_container.page id_o [d]

