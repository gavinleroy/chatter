(** Basic unit tests for Chatter. *)

open Eio.Std

(* Utilities *)

let on_domain ~domain_mgr ~sw thunk =
  Fiber.fork_daemon ~sw @@ fun () ->
  Eio.Domain_manager.run domain_mgr @@ fun () ->
  thunk ();
  `Stop_daemon


let server_addr = `Tcp (Eio.Net.Ipaddr.V4.any, 8080)
let client_addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8080)

(* Tests *)

(* NOTE: this is a stress test of sending/receiving lots of messages. *)
let test_both_to_both () =
  Eio_main.run @@ fun env ->
  let num_messages = 10_000 in
  let server_buffer = Buffer.create (num_messages * 15)
  and client_buffer = Buffer.create (num_messages * 15) in

  let client_out = Eio.Flow.buffer_sink client_buffer
  and server_out = Eio.Flow.buffer_sink server_buffer
  and client_in = Eio_mock.Flow.make "clientin"
  and client_err = Eio_mock.Flow.make "clienterr"
  and server_in = Eio_mock.Flow.make "serverin"
  and server_err = Eio_mock.Flow.make "servererr" in

  let net = Eio.Stdenv.net env
  and domain_mgr = Eio.Stdenv.domain_mgr env
  and clock = Eio.Stdenv.mono_clock env
  and ui_client = Lib.Ui.make_console ~testing:true
      ~stdout:client_out ~stdin:client_in ~stderr:client_err ()
  and ui_server = Lib.Ui.make_console ~testing:true
      ~stdout:server_out ~stdin:server_in ~stderr:server_err ()
  and messages =
    List.init num_messages (fun i -> Printf.sprintf "(message %d)\n" i) in

  let reply, resolve_reply = Promise.create ()
  and return_messages = List.map (fun m -> `Return m) messages in

  return_messages @ [
    `Await reply; `Raise End_of_file;
  ] |> Eio_mock.Flow.on_read server_in;

  return_messages @ [
    `Run (fun () -> Eio.Time.Mono.sleep clock 0.5; "");
    `Raise End_of_file;
  ] |> Eio_mock.Flow.on_read client_in;

  Switch.run @@ fun sw ->
  on_domain ~sw ~domain_mgr (fun () ->
      Lib.Server.run ~domain_mgr ~net
        ~addr:server_addr ~ui:ui_server ~clock);
  on_domain ~sw ~domain_mgr (fun () ->
      Lib.Client.run ~domain_mgr ~net
        ~addr:client_addr
        ~ui:ui_client ~clock;
      Promise.resolve_ok resolve_reply "");

  let _str = Promise.await_exn reply
  and expect = (String.concat "" messages) in
  Alcotest.(check string) "same string"
    expect (Buffer.contents client_buffer);
  Alcotest.(check string) "same string"
    expect (Buffer.contents server_buffer)


let test_client_improper_handshake () =
  Eio_mock.Backend.run @@ fun () ->
  let net = Eio_mock.Net.make "mocknet"
  and flow = Eio_mock.Flow.make "flow"
  and listening_socket = Eio_mock.Net.listening_socket "mocksock" in

  (* Get the flow from listening on a given port *)
  Eio_mock.Net.on_listen net [
    `Return listening_socket
  ];
  Eio_mock.Net.on_accept listening_socket [
    `Return (flow, server_addr)
  ];
  Eio_mock.Flow.on_read flow [
    `Return "bad";
    `Raise End_of_file;
  ];

  (* Simulate running handshake as a client *)
  Switch.run @@ fun sw ->
  (* Start the server listening on a port *)
  let listening_on = Eio.Net.listen net ~sw server_addr ~backlog:0 in
  let socket, _ = Eio.Net.accept listening_on ~sw in
  let did_succeed = Lib.Protocol.run_handshake_seq ~socket
      Lib.Protocol.server_handshake_seq in
  Alcotest.(check bool) "same bool" false did_succeed


let test_server_improper_handshake () =
  Eio_mock.Backend.run @@ fun () ->
  let net = Eio_mock.Net.make "mocknet"
  and flow = Eio_mock.Flow.make "flow" in
  (* On connect with the server we return the mock flow. *)
  Eio_mock.Net.on_connect net [
    `Return flow
  ];
  Eio_mock.Flow.on_read flow [
    `Return "bad";
    `Raise End_of_file;
  ];
  (* Simulate running handshake as a client *)
  Switch.run @@ fun sw ->
  let socket = Eio.Net.connect ~sw net server_addr in
  let did_succeed = Lib.Protocol.run_handshake_seq ~socket
      Lib.Protocol.client_handshake_seq in
  Alcotest.(check bool) "same bool" false did_succeed


let test_multiple_clients () =
  Eio_main.run @@ fun env ->
  let client_out = Eio_mock.Flow.make "clientout"
  and client_in = Eio_mock.Flow.make "clientin"
  and client_err = Eio_mock.Flow.make "clienterr"
  and server_out = Eio_mock.Flow.make "serverout"
  and server_in = Eio_mock.Flow.make "serverin"
  and server_err = Eio_mock.Flow.make "servererr" in

  let net = Eio.Stdenv.net env
  and domain_mgr = Eio.Stdenv.domain_mgr env
  and clock = Eio.Stdenv.mono_clock env
  and ui_client = Lib.Ui.make_console ~testing:true
      ~stdout:client_out ~stdin:client_in ~stderr:client_err ()
  and ui_server = Lib.Ui.make_console ~testing:true
      ~stdout:server_out ~stdin:server_in ~stderr:server_err ()
  in

  let reply, resolve_reply = Promise.create () in
  Eio_mock.Flow.on_read server_in [
    `Await reply;
    `Raise End_of_file;
  ];

  Eio_mock.Flow.on_read client_in [
    `Await reply;
    `Raise End_of_file;
  ];

  Switch.run @@ fun sw ->
  on_domain ~sw ~domain_mgr (fun () ->
      Lib.Server.run ~domain_mgr ~net ~addr:server_addr ~ui:ui_server ~clock);
  on_domain ~sw ~domain_mgr (fun () ->
      Lib.Client.run ~domain_mgr ~net
        ~addr:client_addr ~ui:ui_client ~clock);
  on_domain ~sw ~domain_mgr (fun () ->
      Lib.Client.run ~domain_mgr ~net
        ~addr:client_addr ~ui:ui_client ~clock;
      (* The client should return because it can't connect. Thus, resolving the promise. *)
      Promise.resolve_ok resolve_reply "");

  Eio.Time.Mono.sleep clock 5.; (* 5 seconds is more than the alloted client wait time. *)
  Alcotest.(check (option unit)) "same unit option"
    (Option.Some ())
    (Promise.peek reply |> Option.map (fun _ -> ()))


let test_client_crash () =
  Eio_main.run @@ fun env ->
  let client_out = Eio_mock.Flow.make "clientout"
  and client_in = Eio_mock.Flow.make "clientin"
  and client_err = Eio_mock.Flow.make "clienterr"
  and server_out = Eio_mock.Flow.make "serverout"
  and server_in = Eio_mock.Flow.make "serverin"
  and server_err = Eio_mock.Flow.make "servererr" in

  let net = Eio.Stdenv.net env
  and domain_mgr = Eio.Stdenv.domain_mgr env
  and clock = Eio.Stdenv.mono_clock env
  and ui_client = Lib.Ui.make_console ~testing:true
      ~stdout:client_out ~stdin:client_in ~stderr:client_err ()
  and ui_server = Lib.Ui.make_console ~testing:true
      ~stdout:server_out ~stdin:server_in ~stderr:server_err ()
  in

  let reply, resolve_reply = Promise.create () in
  Eio_mock.Flow.on_read server_in [
    `Await reply;
    `Raise End_of_file;
  ];

  Eio_mock.Flow.on_read client_in [
    `Raise End_of_file;
  ];

  Switch.run @@ fun sw ->
  on_domain ~sw ~domain_mgr (fun () ->
      Lib.Server.run ~domain_mgr ~net ~addr:server_addr ~ui:ui_server ~clock;
      (* The server should remain running after the client disconnects. *)
      Promise.resolve_ok resolve_reply "");
  on_domain ~sw ~domain_mgr (fun () ->
      Lib.Client.run ~domain_mgr ~net
        ~addr:client_addr ~ui:ui_client ~clock);
  Eio.Time.Mono.sleep clock 1.;
  Alcotest.(check (option unit)) "same unit option"
    (Option.None)
    (Promise.peek reply |> Option.map (fun _ -> ()));
  Eio.Time.Mono.sleep clock 1.;
  ignore (Promise.resolve_ok resolve_reply "")



let test_string_encoding () =
  Eio_main.run @@ fun env ->
  let server_buffer = Buffer.create 100 in
  let server_out = Eio.Flow.buffer_sink server_buffer
  and client_out = Eio_mock.Flow.make "clientout"
  and client_in = Eio_mock.Flow.make "clientin"
  and client_err = Eio_mock.Flow.make "clienterr"
  and server_in = Eio_mock.Flow.make "serverin"
  and server_err = Eio_mock.Flow.make "servererr" in

  let net = Eio.Stdenv.net env
  and domain_mgr = Eio.Stdenv.domain_mgr env
  and clock = Eio.Stdenv.mono_clock env
  and ui_client = Lib.Ui.make_console ~testing:true
      ~stdout:client_out ~stdin:client_in ~stderr:client_err ()
  and ui_server = Lib.Ui.make_console ~testing:true
      ~stdout:server_out ~stdin:server_in ~stderr:server_err () in

  let reply, resolve_reply = Promise.create ()
  and message = "❤ʰˏi✌️♠ü\n" in

  Eio_mock.Flow.on_read server_in [
    `Await reply;
    `Raise End_of_file;
  ];

  Eio_mock.Flow.on_read client_in [
    `Return message;
    `Raise End_of_file;
  ];

  Switch.run @@ fun sw ->
  on_domain ~sw ~domain_mgr (fun () ->
      Lib.Server.run ~domain_mgr ~net
        ~addr:server_addr ~ui:ui_server ~clock);
  on_domain ~sw ~domain_mgr (fun () ->
      Lib.Client.run ~domain_mgr ~net
        ~addr:client_addr
        ~ui:ui_client ~clock;
      Promise.resolve_ok resolve_reply "");

  ignore (Promise.await_exn reply);
  Alcotest.(check string) "same string"
    message (Buffer.contents server_buffer)


let () =
  let open Alcotest in
  run "Tests" [
    "basic-communication", [
      test_case "both-to-both" `Quick test_both_to_both;
    ];

    "ill-behaved", [
      test_case "multiple-clients"            `Slow test_multiple_clients;
      test_case "client-crash"                `Quick test_client_crash;
      test_case "client-improper-handshake"   `Quick test_client_improper_handshake;
      test_case "server-improper-handshake"   `Quick test_server_improper_handshake;
    ];

    "misc", [
      test_case "unicode chars" `Quick test_string_encoding;
    ]
  ]
