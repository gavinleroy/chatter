(lang dune 3.4)

(name chatter)
(version 0.1)
(license MIT)
(authors "Gavin Gray <gavinleroy6@gmail.com>")
(maintainers "Gavin Gray <gavinleroy6@gmail.com>")

(source (github gavinleroy/chatter))

(generate_opam_files true)
(formatting
 (enabled_for dune))

(package 
  (name chatter)
  (synopsis "peer-to-peer chat application using Eio")
  (depends
    cmdliner
    mtime
    ipaddr
    (alcotest :with-test)
    (eio_main (= 0.13))
    (ocaml (= 5.1.1))
    dune))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
