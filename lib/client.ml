open Eio.Std


let run ~domain_mgr ~net ~addr ~ui ~clock =
  let module Ui = (val ui : Ui.Ui) in
  Switch.run @@ fun sw ->
  (* NOTE: Timeout connection after 2 seconds,
     this might be a little tight, but for such a simple connection
     it should be okay. *)
  let timeout = Eio.Time.Timeout.seconds clock 2. in
  try
    let socket = Eio.Net.connect ~sw net addr in
    match Eio.Time.Timeout.run timeout (fun () ->
        Protocol.run_handshake_seq ~socket
          Protocol.client_handshake_seq |> Result.ok)
    with
    | Result.Error `Timeout ->
        Ui.info "Connection timedout ... aborting."
    | Result.Ok false ->
        Ui.info "Couldn't connect with server ... aborting."
    | Result.Ok true ->
        let remote_name = Format.asprintf "%a" Eio.Net.Sockaddr.pp addr in
        Chat.start_chat ~sw ~domain_mgr ~socket ~ui ~clock ~remote_name
        |> Protocol.error_msg_to_string
        |> Ui.info
  with
    _ -> Ui.info "Server unavailable ... aborting."
