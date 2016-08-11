[%%shared
 type add_msg = float * string
[@@deriving json]
 type msg = string * float * string
[@@deriving json]
]

(* notification *)

module Notif = Eba_notif.Make(struct
    type key = unit
    [@@deriving eq]
    type notification = int
end)

(* DB operations *)

let (db : msg Ocsipersist.table Lwt.t) =
  Ocsipersist.open_table "messages"

let last_key =
  Eliom_reference.eref
    ~persistent:"index"
    ~scope:Eliom_common.global_scope (-1)

let get id =
  let%lwt db = db in
  Ocsipersist.find db (string_of_int id)

let get_indices () =
  let%lwt index = Eliom_reference.get last_key in
  let rec aux n l = if n > index then l else aux (n+1) (n::l) in
  Lwt.return (aux 0 [])

let username uid =
  let%lwt u = Eba_user.user_of_userid uid in
  Lwt.return @@ (Eba_user.firstname_of_user u) ^ " " ^ (Eba_user.lastname_of_user u)

let add id m s =
  let%lwt db = db in
  let%lwt i = Eliom_reference.get last_key in
  let i = i + 1 in
  let%lwt () = Eliom_reference.set last_key i in
  let%lwt username = username id in
  let%lwt () = Ocsipersist.add db (string_of_int i) (username, m, s) in
  Lwt.return i

let add m s =
  let%lwt i = (Eba_session.connected_fun add) m s in
  let f uid = Lwt.return (Some i) in
  Notif.notify () f;
  Lwt.return i

(* register RPCs *)

let%client add =
  let add_rpc =
    ~%(Eliom_client.server_function [%derive.json : add_msg]
	 (Eba_session.connected_rpc @@ fun _ (m, s) -> add m s))
  in
  fun m s -> add_rpc (m, s)

let%client get =
  let get_rpc =
    ~%(Eliom_client.server_function [%derive.json : int]
	 (Eba_session.connected_rpc @@ fun _ m -> get m))
  in
  fun m -> get_rpc m

(* cache *)
let db_cache : (int,msg) Eliom_cscache.t =
  Eliom_cscache.create ()

let%shared get = Eliom_cscache.find ~%db_cache get

