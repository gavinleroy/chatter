(* use buffered reading / writing globally *)
module Read = Eio.Buf_read
module Write = Eio.Buf_write

(* All internal reading / writing done in big endian *)
module ReadInt = Read.BE
module WriteInt = Write.BE


(* Message header type tags *)
let acknowledge_tag = 0b1111
let message_tag     = 0b1110
let handshake_tag   = 0b1101


(* Handshake protocol messages *)
let handshake_server_to_client_req = 0b1100
let handshake_client_to_server_ok  = 0b1011
let handshake_server_to_client_ok  = 0b1010


(* Unexpected message tag, (expected, actual) *)
exception UnexpectedTag of int

(* Invalid handshake sequence *)
exception InvalidHandshake


(* Handshake network operations *)

let client_handshake_seq =
  [ `Recv, `HandshakeRequest
  ; `Send, `HandshakeClientOk
  ; `Recv, `HandshakeServerOk
  ]


let server_handshake_seq =
  [ `Send, `HandshakeRequest
  ; `Recv, `HandshakeClientOk
  ; `Send, `HandshakeServerOk
  ]


let handshake_body_code = function
  | `HandshakeRequest -> handshake_server_to_client_req
  | `HandshakeClientOk -> handshake_client_to_server_ok
  | `HandshakeServerOk -> handshake_server_to_client_ok


let send_handshake msg ~socket =
  let msg_body = handshake_body_code msg in
  Write.with_flow socket @@ fun outbound ->
  Write.uint8 outbound handshake_tag;
  Write.uint8 outbound msg_body


let receive_handshake expected ~socket =
  let expected_body = handshake_body_code expected in
  let inbound = Read.of_flow socket ~max_size:8 in
  let tag = Read.uint8 inbound in
  if tag <> handshake_tag then
    raise (UnexpectedTag tag);
  let body = Read.uint8 inbound in
  if expected_body <> body then
    raise InvalidHandshake


let run_handshake_seq ~socket ms =
  List.iter (function
    | `Send, msg -> send_handshake msg ~socket
    | `Recv, msg -> receive_handshake msg ~socket) ms


(* Network operations *)

let send_message n bytes ~socket =
  let msg_len = Stdlib.Bytes.length bytes in
  Write.with_flow socket @@ fun outbound ->
  Write.uint8 outbound message_tag;
  WriteInt.uint16 outbound n;
  WriteInt.uint16 outbound msg_len;
  Write.bytes outbound ~len:msg_len bytes


let send_acknowledge n ~socket =
  Write.with_flow socket @@ fun outbound ->
  Write.uint8 outbound acknowledge_tag;
  WriteInt.uint16 outbound n


let receive_acknowledge inbound =
  (* let inbound = Read.of_flow socket ~max_size:8 in *)
  (* let tag = Read.uint8 inbound in *)
  (* if tag <> acknowledge_tag then *)
  (*   raise (UnexpectedTag tag); *)
  ReadInt.uint16 inbound


let receive_message inbound =
  (* let inbound = Read.of_flow socket ~max_size:1_000_000 in *)
  (* let tag = Read.uint8 inbound in *)
  (* if tag <> message_tag then *)
  (*   raise (UnexpectedTag tag); *)

  let id = ReadInt.uint16 inbound in
  let msg_len = ReadInt.uint16 inbound in
  id, (Read.take msg_len inbound |> Bytes.of_string)


let receive ~socket =
  let inbound = Read.of_flow socket ~max_size:1_000_000 in
  let tag = Read.uint8 inbound in
  if tag = acknowledge_tag then
    `Acknowledge (receive_acknowledge inbound)
  else if tag = message_tag then
    `Message (receive_message inbound)
  else
    raise (UnexpectedTag tag)


(* let send bytes ~socket ~clock = *)
(*   let time_before = Eio.Time.now clock in *)
(*   send_message bytes ~socket; *)
(*   receive_acknowledge ~socket; *)
(*   let time_after = Eio.Time.now clock in *)
(*   time_after -. time_before *)


(* let receive ~socket = *)
(*   let bs = receive_message ~socket in *)
(*   send_acknowledge ~socket; *)
(*   bs *)


(* let send_string str ~socket ~clock = *)
(*   Stdlib.Bytes.of_string str *)
(*   |> send ~socket ~clock *)


(* let receive_string ~socket = *)
(*   receive ~socket *)
(*   |> Stdlib.String.of_bytes *)
