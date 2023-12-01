open Eio.Std

let traceln fmt = traceln ("[TRACE server]: " ^^ fmt)

(* Server handshake send / receive *)

let rec server_logic ~socket =
  let message = Protocol.receive_string ~socket in
  traceln "Received from client: %S" message;
  server_logic ~socket


let run_as_server ~socket =
  try
    Protocol.run_handshake_seq ~socket
      Protocol.server_handshake_seq;
    server_logic ~socket
  with
    Protocol.InvalidHandshake ->
      traceln "Ignoring client, invalid handshake"


let run ~net ~addr =
  traceln "Accepting connections on %a" Eio.Net.Sockaddr.pp addr;
  Switch.run @@ fun sw ->
  let listening_on = Eio.Net.listen net ~sw addr ~backlog:5 in
  while true do
    (* NOTE: use `accept` to ensure single connections at a time.
       To allow multiple clients to connect, `accept_fork`,
       which starts a handler in a new domain, could be used. *)
    let socket, _ = Eio.Net.accept listening_on ~sw in
    Fiber.fork ~sw @@ fun () ->
    run_as_server ~socket
  done
