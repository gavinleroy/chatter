# Chatter

A peer-to-peer chat client application. Please, never use this for anything. The main application logic 
lives in [lib/chat.ml](lib/chat.ml), because it's a peer-to-peer chat this is the same for both client and server.

## Building

Install required dependencies using `opam install --deps-only .`, then use `dune build`. This 
application uses the experimental [Eio](https://github.com/ocaml-multicore/eio) library for 
multicore OCaml. Thus, the version of OCaml is pinned to 5.1.1 (the current latest release). The application 
was tested on MacOS (Monterey 12.6.1 x86 silicon) and Linux (Manjaro).

## Running

Use the following to launch the server, `-p` can be used to specify the given port (default `8080`):

``` sh
dune exec bin/chatter.exe -- server -p 8080
```

Connecting a client is equally as easy, IPV4 host can be specified with `-h` and port again with `-p` (defaults `127.0.0.1` and `8080` respectively):

``` sh
dune exec bin/chatter.exe -- client -h 127.0.0.1 -p 8080
```

Only a single client may connect at a time, additional clients will time out after a few seconds rather than 
waiting for the server to be free. The chat UI simply uses the console, this can make incoming/outgoing messages
a little cluttered, even the most basic GUI would improve the issue. I've opted to not spend my time on that.

## Testing

If you wish to run tests, install the dependencies using `opam install --deps-only --with-test .`. After 
installing dependencies, run the test suite with `dune runtests`.

> :warning: the tests take a few seconds to run. Some require the network, so if this is unavailable or if an application is using port 8080 this will interfere.

Enjoy :beers:,
Gavin
