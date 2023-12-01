open Cmdliner


(* FIXME: is this really ok? *)
let server_main _port =
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8080) in
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  Lib.Server.run ~net ~addr


(* FIXME: is this really ok? *)
let client_main _addr _port =
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8080) in
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env
  and stdin = Eio.Stdenv.stdin env
  and stdout = Eio.Stdenv.stdout env
  and clock = Eio.Stdenv.clock env in
  (* Test the server: *)
  Lib.Client.run ~net ~addr ~stdin ~stdout ~clock


(*** ********************** ***)
(*** Command-line interface ***)
(*** ********************** ***)

let version = "0.1"

let port =
  let doc = "Port number to listen on." in
  Arg.(value & opt int 8080 & info ["p"; "port"] ~doc)


let host =
  let doc = "Host to connect to." in
  Arg.(value & opt string "127.0.0.1" & info ["h"; "host"] ~doc)


let server_cmd =
  let doc = "Start Chatter server" in
  let info = Cmd.info "server" ~version ~doc in
  Cmd.v info Term.(const server_main $ port)


let client_cmd =
  let doc = "Start Chatter client" in
  let info = Cmd.info "client" ~version ~doc in
  Cmd.v info Term.(const client_main $ host $ port)


let main_cli =
  let name = "chatter.exe" in
  let doc = "Chatter, a simple one-on-one chat application." in
  let info = Cmd.info name ~doc in
  Cmd.group info [server_cmd; client_cmd]

let () =
  Stdlib.exit (Cmd.eval main_cli)
