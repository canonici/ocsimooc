
let%shared connected_user_box user = Eliom_content.Html.D.(
  let username = Os_view.username user in
  div ~a:[a_class ["connected-user-box"]] [
    Os_view.avatar user;
    div [
      username
    ];
    Ocsimooc_content.Settings.settings_button ();
    Ocsimooc_content.Connection.disconnect_button ();
  ]
)

let%shared connection_box () = Eliom_content.Html.D.(
  let%lwt sign_in    = Ocsimooc_content.Connection.sign_in_button () in
  let%lwt sign_up    = Ocsimooc_content.Connection.sign_up_button () in
  let%lwt forgot_pwd = Ocsimooc_content.Connection.forgotpwd_button () in
  Lwt.return @@ div ~a:[a_class ["os-login-menu"]] [
    sign_in;
    sign_up;
    forgot_pwd
  ]
)

let%shared msg () = Eliom_content.Html.D.(
  div ~a:[a_id "os_msg"] []
)

let%shared userbox user = Eliom_content.Html.F.(
  let d = div ~a:[a_class ["navbar-right"]] in
  let msg = msg () in
  match user with
  | None ->
    let%lwt cb = connection_box () in
    Lwt.return @@ d [msg; cb]
  | Some user ->
    Lwt.return @@ d [msg; connected_user_box user]
)
