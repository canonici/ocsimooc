(* This file was generated by Eliom-base-app.
   Feel free to use it, modify it, and redistribute it as you wish. *)

[%%shared
    open Eliom_content.Html.D
]

[%%shared
let () =
  (* Registering services. Feel free to customize handlers. *)
  Eliom_registration.Action.register
    ~service:Os_services.set_personal_data_service'
    Ocsimooc_handlers.set_personal_data_handler';

  Eliom_registration.Action.register
    ~service:Os_services.set_password_service'
    Ocsimooc_handlers.set_password_handler';

  Eliom_registration.Action.register
    ~service:Os_services.forgot_password_service
    Ocsimooc_handlers.forgot_password_handler;

  Eliom_registration.Action.register
    ~service:Os_services.preregister_service'
    Ocsimooc_handlers.preregister_handler';

  Eliom_registration.Action.register
    ~service:Os_services.sign_up_service'
    Os_handlers.sign_up_handler;

  Eliom_registration.Unit.register
    ~service:Os_services.connect_service
    Os_handlers.connect_handler;

  Eliom_registration.Unit.register
    ~service:Os_services.disconnect_service
    Os_handlers.disconnect_handler;

  Eliom_registration.Any.register
    ~service:Os_services.activation_service
    Ocsimooc_handlers.activation_handler;

  Eliom_registration.Action.register
    ~service:Os_services.add_mail_service
    Os_handlers.add_mail_handler;

  Ocsimooc_base.App.register
    ~service:Ocsimooc_services.about_service
    (Ocsimooc_page.Opt.connected_page Ocsimooc_handlers.about_handler);

  Ocsimooc_base.App.register
    ~service:Ocsimooc_services.settings_service
    (Ocsimooc_page.Opt.connected_page Ocsimooc_handlers.settings_handler);

  Ocsimooc_base.App.register
    ~service:Os_services.main_service
    (Ocsimooc_page.Opt.connected_page Ocsimooc_handlers.main_service_handler)
]

let () =

  Eliom_registration.Ocaml.register
    ~service:Ocsimooc_services.upload_user_avatar_service
    (Os_session.connected_fun Ocsimooc_handlers.upload_user_avatar_handler)



(* Print more debugging information when <debugmode/> is in config file
   (DEBUG = yes in Makefile.options).
   Example of use:
   let section = Lwt_log.Section.make "Ocsimooc:sectionname"
   ...
   Lwt_log.ign_info ~section "This is an information";
   (or ign_debug, ign_warning, ign_error etc.)
 *)
let _ =
  if Eliom_config.get_debugmode ()
  then begin
    ignore
      [%client (
        (* Eliom_config.debug_timings := true; *)
        (* Lwt_log_core.add_rule "eliom:client*" Lwt_log.Debug; *)
        (* Lwt_log_core.add_rule "os*" Lwt_log.Debug; *)
        Lwt_log_core.add_rule "Ocsimooc*" Lwt_log.Debug
        (* Lwt_log_core.add_rule "*" Lwt_log.Debug *)
        : unit ) ];
    (* Lwt_log_core.add_rule "*" Lwt_log.Debug *)
    Lwt_log_core.add_rule "Ocsimooc*" Lwt_log.Debug
  end
