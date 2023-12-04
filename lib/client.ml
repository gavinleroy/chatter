open Eio.Std

let run ~domain_mgr ~net ~addr ~stdin ~stdout ~clock =
  traceln "Connecting to server at %a..." Eio.Net.Sockaddr.pp addr;
  Switch.run @@ fun sw ->
  try
    (* TODO: use a timeout here, and abort if the server isn't available. *)
    let socket = Eio.Net.connect ~sw net addr in
    Protocol.run_handshake_seq ~socket
      Protocol.client_handshake_seq;
    Chat.start_chat
      ~sw
      ~domain_mgr
      ~socket
      ~stdout
      ~stdin
      ~clock
  with
  | Protocol.InvalidHandshake ->
      traceln "Couldn't complete handshake with server, aborting ..."
  | e ->
      traceln "An error occurred %a" Fmt.exn e
