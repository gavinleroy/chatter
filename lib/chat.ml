(** This file contains the main logic of a peer-to-peer chat application.
    For example, both client and server write messages,
    and receive messages over both a network and frontend UI.

    The application has four working threads (domains), each with a specific task:

    1. Reading from the network.
    2. Writing to the network.
    3. Reading from the UI.
    4. Writing to the UI.

    Domains that need to communicate with each other do so over a stream. I've
    made the decision to use zero-sized streams, this causes the put operation to
    wait until the receiving worker is ready to process the request. This ensures
    that no thread gets too far ahead in its work.

    The only domains that receive messages on a channel are the two writers
    (to the network and UI). The message types, and the data they contain are
    outlined in the [mailboxes] type. Each message is identified with a unique
    identifier, this identifier is generated the UI reader and each read message
    is marked with a fresh id. Ids are used in both messages and acknowledgments, see the
    [Protocol] module for information about how they are serialized / deserialized. *)
open Eio.Std

module Stream = Eio.Stream
module Read = Eio.Buf_read
module Write = Eio.Buf_write

type mailboxes =
  { netout : [ `Message of (Protocol.local_msg_id * Bytes.t)
             | `Acknowledge of Protocol.remote_msg_id ] Stream.t

  ; uiout : [ `Prompt of Protocol.local_msg_id
             | `Message of Bytes.t
             | `Timestamp of (Protocol.local_msg_id * Mtime.t)
             | `Acknowledged of (Protocol.local_msg_id * Mtime.t) ] Stream.t

  ; exit : [ `Exit of [
      | `Send_fail
      | `Recv_fail
      | `Invalid_id
      | `Ui_read_fail
      | `Ui_write_fail
    ] ] Stream.t
  }


(* Just to make working with the options a little nicer. *)
open (struct
  let ( let* ) = Option.bind
  let ( let+ ) f s = Option.map s f
  let return = Option.some
end)


let run_network_writer loop_until mailboxes socketout =
  (* Use buffered output *)
  Write.with_flow socketout @@ fun flowout ->
  loop_until `Send_fail @@ fun () ->
  let request = Eio.Stream.take mailboxes.netout in
  match request with
  | `Message (id, bytes) ->
      traceln "sending %d" (let (Local id) = id in id);
      Protocol.send_message id bytes flowout
  | `Acknowledge id ->
      traceln "acknowledging %d" (let (Remote id) = id in id);
      Protocol.send_acknowledge id flowout


let run_network_reader loop_until mailboxes socketin clock =
  (* Using buffered input *)
  let flowin = Read.of_flow socketin ~max_size:1_000_000 in
  loop_until `Recv_fail @@ fun () ->
  let+ request = Protocol.receive flowin in
  match request with
  | `Message (id, bytes) ->
      traceln "received %d" (let (Remote id) = id in id);
      Stream.add mailboxes.netout (`Acknowledge id);
      Stream.add mailboxes.uiout (`Message bytes)
  | `Acknowledge id ->
      traceln "acknowledged %d" (let (Local id) = id in id);
      Stream.add mailboxes.uiout (`Acknowledged (id, Eio.Time.Mono.now clock))


let run_ui_writer loop_until mailboxes ui remote_name =
  let module Ui_write = (val ui : Ui.Ui) in
  (* NOTE: Track the timestamps of the local message ids.
     Acknowledgements of unrecognized ids cause the chat to exit. *)
  let (push_timestamp, pop_timestamp) =
    let timestamps = Hashtbl.create 10 in
    (fun id time -> Hashtbl.add timestamps id time),
    (fun id ->
       let+ sent_time = Hashtbl.find_opt timestamps id in
       Hashtbl.remove timestamps id;
       sent_time)
  in
  loop_until `Ui_write_fail @@ fun () ->
  let request = Stream.take mailboxes.uiout in
  match request with
  | `Prompt id ->
      Ui_write.prompt_message id
  | `Timestamp (id, start_time) ->
      push_timestamp id start_time;
      return ()
  | `Message line ->
      Ui_write.received_message remote_name line
  | `Acknowledged (id, end_time) ->
      let* start_time = pop_timestamp id in
      let roundtrip = Mtime.span start_time end_time in
      Ui_write.acknowledge_message id roundtrip


let run_ui_reader loop_until mailboxes ui clock =
  let module Ui_read = (val ui : Ui.Ui) in
  let fresh_id =
    let bx = ref 0 in fun () -> incr bx; Protocol.Local !bx
  in
  loop_until `Ui_read_fail @@ fun () ->
  let msg_id = fresh_id () in
  Stream.add mailboxes.uiout (`Prompt msg_id);
  let+ line = Ui_read.get_message () in
  Stream.add mailboxes.uiout (`Timestamp (msg_id, Eio.Time.Mono.now clock));
  Stream.add mailboxes.netout (`Message (msg_id, line))


let start_chat ~sw ~domain_mgr ~remote_name ~socket ~ui ~clock =
  let mailboxes =
    { netout = Stream.create 0
    ; uiout  = Stream.create 0
    ; exit   = Stream.create 0
    }

  and on_domain thunk =
    Fiber.fork_daemon ~sw @@ fun () ->
    Eio.Domain_manager.run domain_mgr @@ fun () ->
    thunk ();
    `Stop_daemon

  in

  (* Each domain worker has the signature [unit -> unit option], when the
     result is an [Option.None] the loop will terminate, passing the specified
     exit [msg] to the main thread. *)
  let rec loop_until msg f =
    f () |> function
    | Option.Some () -> loop_until msg f
    | Option.None -> Stream.add mailboxes.exit (`Exit msg)
  in

  on_domain (fun () -> run_network_writer loop_until mailboxes socket);
  on_domain (fun () -> run_ui_writer loop_until mailboxes ui remote_name);
  on_domain (fun () -> run_network_reader loop_until mailboxes socket clock);
  on_domain (fun () -> run_ui_reader loop_until mailboxes ui clock);

  match Stream.take mailboxes.exit with
  | `Exit msg -> msg
