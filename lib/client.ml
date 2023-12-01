open Eio.Std

(* Prefix all trace output with "client: " *)
let traceln fmt = traceln ("client: " ^^ fmt)

module Read = Eio.Buf_read
module Write = Eio.Buf_write


let float_to_time =
  Printf.sprintf "%f"


let client_logic ~socket ~stdin ~stdout ~clock =
  let tin = Read.of_flow stdin ~initial_size:100 ~max_size:1_000_000 in
  Write.with_flow stdout @@ fun tout ->
  while true do
    Write.string tout "> ";
    let line = Read.line tin in
    let time = Protocol.send_string line ~socket ~clock in

    (* HACK: fixme *)
    Write.char tout '\n';
    Write.string tout "Roundtrip: ";
    Write.string tout (float_to_time time);
    Write.char tout '\n'
  done


let start_connection ~socket ~stdin ~stdout ~clock =
  try
    Protocol.run_handshake_seq ~socket
      Protocol.client_handshake_seq;
    client_logic ~socket ~stdin ~stdout ~clock
  with
  | Protocol.InvalidHandshake ->
      traceln "Couldn't complete handshake with server, aborting ..."


(* Connect to [addr] on [net], send a message and then read the reply. *)
let run ~net ~addr ~stdin ~stdout ~clock =
  traceln "Connecting to server at %a..." Eio.Net.Sockaddr.pp addr;
  Switch.run @@ fun sw ->
  let socket = Eio.Net.connect ~sw net addr in
  start_connection ~socket ~stdin ~stdout ~clock
