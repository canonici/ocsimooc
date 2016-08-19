let%server application_name = !Ocsimooc_config.app_name

let%client application_name = Eliom_client.get_application_name ()

let%shared displayed_app_name = "ocsimooc"

let () =
  let int_of_pgport s =
    try
      int_of_string s
    with Failure _ ->
      failwith @@ Printf.sprintf
        "PGPORT environment variable must be an integer, not '%s'" s
  in
  Os_db.init
    ?host:!Ocsimooc_config.eba_db_host
    ?port:!Ocsimooc_config.eba_db_port
    ?user:!Ocsimooc_config.eba_db_user
    ?password:!Ocsimooc_config.eba_db_password
    ?database:!Ocsimooc_config.eba_db_database
    ?unix_domain_socket_dir:!Ocsimooc_config.eba_db_unix_domain_socket_dir
    ()

let () = Os_email.set_mailer "/usr/sbin/sendmail"

[%%shared
module App = Eliom_registration.App(struct
    let application_name = application_name
    let global_data_path = Some ["__global_data__"]
  end)
]
