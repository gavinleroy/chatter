# Chatter

A one-on-one chat client application. Please, never use this for anything.

## Building

This application was tested using OCaml 5.1.0 on MacOS (Monterey 12.6.1 x86 silicon) and Linux (Manjaro). Simply run `dune build` to build the application.

## Running

Use the following to launch the server, `-p` can be used to specify the given port:

``` sh
dune exec bin/chatter.exe -- server -p 8080
```

Connecting a client is equally as easy, IPV4 host can be specified with `-h` and port again with `-p`:

``` sh
dune exec bin/chatter.exe -- client -h 127.0.0.1 -p 8080
```

## Testing

Use the command `dune runtests tests`.

Enjoy :beers:,
Gavin

## Test task

> The task description is currently included for ease-of-reading, it will be removed.

Simple one on one chat.

Application should start in two modes:

- as a server, waiting for one client to connect or
- as a client, taking an IP address (or hostname) of server to connect to

After connection is established, user on either side (server and client) can send messages to the other side. After connection is terminated by the client, server continues waiting for another client. The receiving side should acknowledge every incoming message (automatically send back "message received" indication), sending side should show the roundtrip time for acknowledgment. Wire protocol shouldn't make any assumptions on the message contents (e.g. allowed byte values, character encoding, etc).

UI is your choice - can be just a console.

Requirements:

- Application is to be compiled and run on Linux
- Implementation language: OCaml
- Can use any 3rd-party general-purpose libraries (extlib, containers, lwt, etc.)
- Primary objectives: robustness, code simplicity and maintainability

Good luck.
