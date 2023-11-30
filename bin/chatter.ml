open Eio.Std
open Cmdliner

(* (\* Run a server and a test client, communicating using [net]. *\) *)
(* let main ~net ~stdin ~clock = *)
(*   Switch.run @@ fun sw -> *)

(*   (\* We create the listening socket first so that we can be sure it is ready *)
(*      as soon as the client wants to use it. *\) *)
(*   let listening_socket = Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:5 addr in *)

(*   (\* Start the server running in a new fiber. *)
(*      Using [fork_daemon] here means that it will be stopped once the client is done *)
(*      (we don't wait for it to finish because it will keep accepting new connections forever). *\) *)
(*   Fiber.fork_daemon ~sw (fun () -> Lib.Server.run listening_socket); *)

(*   (\* Test the server: *\) *)
(*   Lib.Client.run ~net ~addr ~stdin ~clock *)

(* let () = *)
(*   Eio_main.run @@ fun env -> *)
(*   main ~net:(Eio.Stdenv.net env) ~stdin:(Eio.Stdenv.stdin env) ~clock:(Eio.Stdenv.clock env) *)


(* FIXME: is this really ok? *)
let server_main _port =
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8080) in
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  Switch.run @@ fun sw ->
  (* We create the listening socket first so that we can be sure it is ready
     as soon as the client wants to use it. *)
  let listening_socket = Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:5 addr in
  Fiber.fork_daemon ~sw @@ fun () ->
  Lib.Server.run listening_socket


(* FIXME: is this really ok? *)
let client_main _addr _port =
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8080) in
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env
  and stdin = Eio.Stdenv.stdin env
  and clock = Eio.Stdenv.clock env in
  (* Test the server: *)
  Lib.Client.run ~net ~addr ~stdin ~clock


(*** ********************** ***)
(*** Command-line interface ***)
(*** ********************** ***)


let server_cmd =
  let port =
    let doc = "Port number to listen on." in
    Arg.(value & opt int 8080 & info ["p"; "port"] ~doc)
  in
  Term.(const server_main $ port)


let client_cmd =
  let host =
    let doc = "Host to connect to." in
    Arg.(value & opt string "127.0.0.1" & info ["h"; "host"] ~doc)
  and port =
    let doc = "Port number to connect to." in
    Arg.(required & pos 0 (some int) None & info [] ~docv:"PORT" ~doc)
  in
  Term.(const client_main $ host $ port)


let mode =
  let mode_term =
    let doc = "Choose between client and server mode." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"MODE" ~doc)
  in

  let doc = "Subcommand: server" in
  Term.(const (fun _ -> `Server) $ const ()),

  let doc = "Subcommand: client" in

  Term.(const (fun _ -> `Client) $ const ()),

  let cmds = [
    ("server", (Cmd.info "server" ~doc ~sdocs:"COMMON OPTIONS" ~exits:Cmd.Exit.defaults)),
    ("client", (Cmd.info "client" ~doc ~sdocs:"COMMON OPTIONS" ~exits:Cmd.Exit.defaults))
  ]
  in

  Term.(ret (const (`Help (`Pager, None)))),
  Term.(const (fun mode () ->
      match mode with
      | "server" -> server_cmd
      | "client" -> client_cmd
      | _ -> exit 1)
    $ mode_term)


(* let mode = *)
(*   let doc = "Choose between client and server mode." in *)
(*   let server = Term.(const (`Server server_main) $ const ()) in *)
(*   let client = Term.(const (`Client client_main) $ const ()) in *)
(*   Arg.(value & vflag `Server [(`Server, server); (`Client, client)] & info ["mode"] ~doc) *)


let default_cmd =
  let doc = "Simple one-on-one chat application." in
  let man = [
    `S "DESCRIPTION";
    `P "Simple one-on-one chat application using OCaml.";
    (* Add more manual information here if needed *)
  ] in
  Term.(ret (const (`Help (`Pager, None)))),
  Cmd.info "chat" ~version:"1.0" ~doc ~man


let () =
  let cmds = [server_cmd; client_cmd] in
  match Cmd.group default_cmd cmds ~catch:true with
  | `Error _ -> exit 1
  | _ -> exit 0
