open Eio.Std

module Write = Eio.Buf_write

(* TODO: handle broken pipes and closing clients, the server needs to stay alive. *)

(* Uses of [traceln] should be replaced by proper prints. *)

let try_handshake socket =
  try
    Protocol.run_handshake_seq ~socket
      Protocol.server_handshake_seq;
    true
  with
    Protocol.InvalidHandshake -> false


let run ~domain_mgr ~net ~addr ~stdin ~stdout ~clock =
  traceln "Accepting connections on %a" Eio.Net.Sockaddr.pp addr;
  Switch.run @@ fun sw ->
  let listening_on = Eio.Net.listen net ~sw addr ~backlog:5 in
  while true do
    traceln "Waiting for client.";
    (* NOTE: use `accept` to ensure single connections at a time.
       To allow multiple clients to connect, `accept_fork`,
       which starts a handler in a new domain, could be used. *)
    let socket, _ = Eio.Net.accept listening_on ~sw in
    begin
      if try_handshake socket then
        traceln "Client connected! Launching chat..."
      else
      traceln "Handshake failed, ignoring client";
      try
        Chat.start_chat
          ~sw
          ~domain_mgr
          ~socket
          ~stdout
          ~stdin
          ~clock
      with
        _ -> ()
      (* Write.string to_terminal "Client disconnected :(" *)
    end
  done
