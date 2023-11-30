open Eio.Std

(* Prefix all trace output with "client: " *)
let traceln fmt = traceln ("client: " ^^ fmt)

module Read = Eio.Buf_read
module Write = Eio.Buf_write

(* Connect to [addr] on [net], send a message and then read the reply. *)
let run ~net ~addr ~stdin ~clock =
  traceln "Connecting to server at %a..." Eio.Net.Sockaddr.pp addr;

  let from_terminal = Eio.Buf_read.of_flow stdin ~initial_size:100 ~max_size:1_000_000 in

  while true do
    let line = Eio.Buf_read.line from_terminal in

    (* This reconnects to the server everytime a message is to be sent. *)
    (* Ultimately, what we want is a buffered connection between the two *)
    (* and then the messages can just flow from there. *)
    Switch.run @@ fun sw ->
    let flow = Eio.Net.connect ~sw net addr in
    (* We use a buffered writer here so we can create the message in multiple
       steps but still send it efficiently as a single packet: *)
    Write.with_flow flow @@ fun to_server ->
    Write.string to_server line;
    Write.string to_server "\n";

    let time_before = Eio.Time.now clock in
    let reply = Read.(parse_exn take_all) flow ~max_size:100 in
    let time_round = (Eio.Time.now clock) -. time_before in

    match reply with
    | "OK" -> traceln "Got reply %f" time_round
    | _ -> traceln "Error: received wrong message"

  done
