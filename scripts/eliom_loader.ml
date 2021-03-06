(* Load Eliom client-side program after storing global data in
   localStorage. Compile as follos:

   ocamlfind ocamlc \
     -package js_of_ocaml,js_of_ocaml.ppx,lwt.ppx \
     -linkpkg -o eliom_loader.byte \
     eliom_loader.ml

   js_of_ocaml eliom_loader.byte
*)
let debug = false

let log =
  if debug then
    (fun s ->
       Firebug.console##log(Js.string s);
       let p = Dom_html.createP Dom_html.document in
       p##.style##.color := Js.string "blue";
       Dom.appendChild p (Dom_html.document##createTextNode (Js.string s));
       let container = Dom_html.getElementById "app-container" in
       Dom.appendChild container p)
  else
    (fun s -> ())

let update_failed = ref false
let data_upload_failed = ref false

let url =
  Js.Optdef.case (Js.Unsafe.global##.___eliom_server_)
    (fun ()     -> "127.0.0.1:8080/__global_data__")
    (fun server -> Js.to_string server ^ "/__global_data__")

let storage () =
  Js.Optdef.case (Dom_html.window##.localStorage)
    (fun () -> failwith "Browser storage not supported")
    (fun v -> v)

let rec add_retry_button wake msg =
  let container = Dom_html.getElementById "app-container" in
  let p = Dom_html.createP Dom_html.document in
  let btn = Dom_html.createButton Dom_html.document in
  (* Set error class *)
  (Dom_html.getElementById "app-container")##.className :=
    Js.string "app-error";
  (* Error message paragraph *)
  Dom.appendChild p
    (Dom_html.document##createTextNode
      (Js.string (msg ^ ". Please try again later.")));
  p##.id := Js.string "retry-message";
  (* Retry button *)
  Dom.appendChild btn
    (Dom_html.document##createTextNode(Js.string "Retry"));
  btn##.onclick := Dom_html.handler
      (fun _ ->
         Dom.removeChild container p;
         if !update_failed then begin
           update_failed := false;
           ignore (Js.Unsafe.global##.chcp##fetchUpdate)
         end;
         if !data_upload_failed then begin
           data_upload_failed := false;
           Lwt.async (fun () -> get_data wake)
         end;
         Js._false);
  btn##.id := Js.string "retry-button";
  Dom.appendChild p btn;
  Dom.appendChild container p

and get_data wake =
  let%lwt {XmlHttpRequest.content; code} = XmlHttpRequest.get url in
  if code = 200 then begin
log "Got global data";
    (storage ())##setItem (Js.string "__global_data") (Js.string content);
    Lwt.wakeup wake ()
  end else begin
log "Could not get global data";
    if not (!update_failed || !data_upload_failed) then begin
      data_upload_failed := true;
      add_retry_button wake "No connection available"
    end;
  end;
  Lwt.return ()

let redirect () =
  Js.Optdef.iter (Js.Unsafe.global##.___eliom_html_url_)
    (fun url -> Dom_html.window##.location##replace (url))

let _ =
  (* CHCP does not run in the background, so we check for updates on resume *)
  ignore @@ Dom.addEventListener Dom_html.document
    (Dom_html.Event.make "resume")
    (Dom.handler (fun _ ->
       log "Resume";
                    ignore (Js.Unsafe.global##.chcp##fetchUpdate);
                    Js._true))
    Js._false;
  let wait2, wake2 = Lwt.wait () in
  let callback ev =
    Dom.handler
      (fun _ ->
log ev;
         update_failed := false;
         Lwt.wakeup wake2 ();
         Js.bool true)
  in
  List.iter
    (fun ev ->
       ignore @@
       Dom.addEventListener Dom_html.document (Dom_html.Event.make ev)
         (callback ev) Js._false)
    ["chcp_nothingToUpdate"];
  let wait, wake = Lwt.wait () in
  let error_callback name =
    Dom.handler
      (fun ev ->
log (name ^ ": " ^ Js.to_string (ev##.detail##.error##.description));
         update_failed := true;
         if not !data_upload_failed then
           add_retry_button wake
             (Js.to_string (ev##.detail##.error##.description));
         Js.bool true)
  in
  List.iter
    (fun ev ->
       ignore @@
       Dom.addEventListener Dom_html.document (Dom_html.Event.make ev)
         (error_callback ev) Js._false)
    ["chcp_updateLoadFailed";
     "chcp_updateInstallFailed";
     "chcp_assetsInstallationError"];
  let status_callback name =
    Dom.handler
      (fun ev ->
         log name;
         Js.bool true)
  in
  List.iter
    (fun ev ->
       ignore @@
       Dom.addEventListener Dom_html.document (Dom_html.Event.make ev)
         (status_callback ev) Js._false)
    ["chcp_updateIsReadyToInstall";
     "chcp_beforeInstall";
     "chcp_nothingToInstall";
     "chcp_updateInstalled";
     "chcp_beforeAssetsInstalledOnExternalStorage";
     "chcp_assetsInstalledOnExternalStorage"];
  Lwt.async @@ fun () ->
  let%lwt _ = Lwt_js_events.onload () in
  let%lwt _ = get_data wake in
  let%lwt _ = wait in
  let%lwt _ = wait2 in
  Lwt.return (redirect ())
