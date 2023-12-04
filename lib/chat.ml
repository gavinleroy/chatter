open Eio.Std

module Stream = Eio.Stream
module Read = Eio.Buf_read
module Write = Eio.Buf_write

(* This file contains the main logic of a messenger.
   For example, both client and server write messages,
   and receive messages over both a network and frontend UI. *)

(* FIXME: best to get rid of his, there shouldn't ever
   be syncrhonization errors, and the tests show this. But
   how to encode this in the type system... *)
exception Internal_synchronization

type message_id = int

type timestamp = float

type mailboxes =
    { netout : [ `Message of (message_id * Bytes.t)
               | `Acknowledge of message_id ] Stream.t

    ; stdout : [ `Prompt of message_id
               | `Message of Bytes.t
               | `Timestamp of (message_id * timestamp * unit Promise.u)
               | `Acknowledged of (message_id * timestamp) ] Stream.t

    ; exit : [ `Exit ] Stream.t
    }


let run_network_writer mailboxes socketout =
  while true do
    let request = Eio.Stream.take mailboxes.netout in
    match request with
    | `Message (id, bytes) ->
        Protocol.send_message id bytes ~socket:socketout
    | `Acknowledge id ->
        Protocol.send_acknowledge id ~socket:socketout
  done


let run_network_reader mailboxes socketin clock =
  while true do
    let request = Protocol.receive ~socket:socketin in
    match request with
    | `Message (id, bytes) ->
        Stream.add mailboxes.netout (`Acknowledge id);
        Stream.add mailboxes.stdout (`Message bytes)
    | `Acknowledge id ->
        Stream.add mailboxes.stdout (`Acknowledged (id, Eio.Time.now clock))
  done


let run_ui_writer mailboxes stdout =
  let (set_timestamp, get_timestamp) =
    let timestamps = Hashtbl.create 10 in
    (fun id time -> Hashtbl.add timestamps id time),
    (fun id -> match Hashtbl.find_opt timestamps id with
      | None ->  raise Internal_synchronization
      | Some sent_time ->
          Hashtbl.remove timestamps id;
          sent_time)
  in
  Write.with_flow stdout @@ fun to_terminal ->
  while true do
    let request = Stream.take mailboxes.stdout in
    let newline () = Write.char to_terminal '\n' in
    match request with
    | `Prompt id ->
        Printf.sprintf "(%d)> " id
        |> Write.string to_terminal
    | `Timestamp (id, start_time, reply) ->
        set_timestamp id (start_time, reply)
    | `Message line ->
        begin
          Write.string to_terminal "Message: ";
          Stdlib.String.of_bytes line
          |> Write.string to_terminal;
          newline ();
        end
    | `Acknowledged (id, end_time) ->
        let start_time, reply = get_timestamp id in
        let roundtrip = end_time -. start_time in
        begin
          Printf.sprintf "time: %f" roundtrip
          |> Write.string to_terminal;
          newline ()
          |> Promise.resolve reply
        end
  done


let run_ui_reader mailboxes stdin clock =
  let fresh_id =
    let bx = ref 0 in fun () -> incr bx; !bx
  in
  let from_terminal = Read.of_flow stdin ~initial_size:100 ~max_size:1_000_000 in
  (* TODO is there a better way to get the raw bytes? *)
  while true do
    let msg_id = fresh_id () in
    Stream.add mailboxes.stdout (`Prompt msg_id);
    let line = Read.line from_terminal |> Bytes.of_string in
    let reply, resolve_reply = Promise.create () in
    Stream.add mailboxes.stdout (`Timestamp (msg_id, Eio.Time.now clock, resolve_reply));
    Stream.add mailboxes.netout (`Message (msg_id, line));
    Promise.await reply
  done


let start_chat ~sw ~domain_mgr ~socket ~stdout ~stdin ~clock =
  let mailboxes =
    { netout = Stream.create 0
    ; stdout = Stream.create 0
    ; exit = Stream.create 0
    }
  in
  let on_domain thunk =
    Fiber.fork_daemon ~sw @@ fun () ->
    Eio.Domain_manager.run domain_mgr @@ fun () ->
    thunk ();
    `Stop_daemon
  in
  on_domain (fun () -> run_network_writer mailboxes socket);
  on_domain (fun () -> run_network_reader mailboxes socket clock);
  on_domain (fun () -> run_ui_writer mailboxes stdout);
  on_domain (fun () -> run_ui_reader mailboxes stdin clock);
  match Stream.take mailboxes.exit with
  | `Exit -> ()
