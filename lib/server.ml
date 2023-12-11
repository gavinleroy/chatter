open Eio.Std

module Write = Eio.Buf_write


let run ~domain_mgr ~net ~addr ~ui ~clock =
  let module Ui = (val ui : Ui.Ui) in
  try
    Switch.run @@ fun sw ->
    (* NOTE: Don't allow a backlog, we only accept a single client at a time. *)
    let listening_on = Eio.Net.listen net ~sw addr ~backlog:0 in
    while true do
      Ui.info "Waiting for client...";
      (* NOTE: use `accept` to ensure single connections at a time.
         To allow multiple clients to connect, `accept_fork`,
         which starts a handler in a new domain, could be used. *)
      let socket, addr = Eio.Net.accept listening_on ~sw in
      if Protocol.run_handshake_seq ~socket
          Protocol.server_handshake_seq then
        begin
          let remote_name = Format.asprintf "%a" Eio.Net.Sockaddr.pp addr in
          Ui.info "Client connected!";
          Chat.start_chat ~sw ~domain_mgr ~socket ~ui ~clock ~remote_name
          |> Protocol.error_msg_to_string
          |> Ui.info
        end
      else
      Ui.info "Couldn't connect with client ... dropping connection."
    done
  with
    _ -> Ui.info "Server couldn't bind to port ... aborting."
