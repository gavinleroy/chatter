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
exception UnexpectedTag of (int * int)

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
    raise (UnexpectedTag (handshake_tag, tag));
  let body = Read.uint8 inbound in
  if expected_body <> body then
    raise InvalidHandshake


let run_handshake_seq ~socket ms =
  List.iter (function
    | `Send, msg -> send_handshake msg ~socket
    | `Recv, msg -> receive_handshake msg ~socket) ms


(* Network operations *)

let send_message bytes ~socket =
  let msg_len = Stdlib.Bytes.length bytes in
  Write.with_flow socket @@ fun outbound ->
  Write.uint8 outbound message_tag;
  WriteInt.uint16 outbound msg_len;
  Write.bytes outbound ~len:msg_len bytes


let send_acknowledge ~socket =
  Write.with_flow socket @@ fun outbound ->
  Write.uint8 outbound acknowledge_tag


let receive_acknowledge ~socket =
  let inbound = Read.of_flow socket ~max_size:8 in
  let tag = Read.uint8 inbound in
  if tag <> acknowledge_tag then
    raise (UnexpectedTag (acknowledge_tag, tag))


let receive_message ~socket =
  let inbound = Read.of_flow socket ~max_size:1_000_000 in
  let tag = Read.uint8 inbound in
  if tag <> message_tag then
    raise (UnexpectedTag (message_tag, tag));
  let msg_len = ReadInt.uint16 inbound in
  Read.take msg_len inbound
  |> Stdlib.Bytes.of_string


let send bytes ~socket ~clock =
  let time_before = Eio.Time.now clock in
  send_message bytes ~socket;
  receive_acknowledge ~socket;
  let time_after = Eio.Time.now clock in
  time_after -. time_before


let receive ~socket =
  let bs = receive_message ~socket in
  send_acknowledge ~socket;
  bs


let send_string str ~socket ~clock =
  Stdlib.Bytes.of_string str
  |> send ~socket ~clock


let receive_string ~socket =
  receive ~socket
  |> Stdlib.String.of_bytes
