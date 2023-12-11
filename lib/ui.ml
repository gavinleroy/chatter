(** A module for abstracting over user interfaces. Due to a
    complete lack of time, I've resorted to only using a console UI.
    This of course brings problems when reading / writing as lines can get
    interleaved and I haven't bothered to do any terminal escape code magic. *)


module type Ui = sig

  type username = string


  (** Format information message. *)
  val info : string -> unit


  (** Debug messages to the console. *)
  val debug : ('a, Stdlib.Format.formatter, unit, unit) Stdlib.format4 -> 'a


  (** Give the user a prompt  *)
  val prompt_message : Protocol.local_msg_id -> unit option


  (** Inform the user that a message was received. *)
  val received_message : username -> Bytes.t -> unit option


  (** Get an input message from the user. *)
  val get_message : unit -> Bytes.t option


  (** Inform the user that identified message was acknowledged *)
  val acknowledge_message : Protocol.local_msg_id -> Mtime.span -> unit option

end


(** Create a UI Console isntance given the two resources for [stdout] and [stdin]. *)
let make_console ?(testing = false) ~stdout ~stdin ~stderr () =
  (module struct
    module Write = Eio.Buf_write
    module Read = Eio.Buf_read

    type username = string

    let formatter_of_resource r =
       Format.make_formatter
         (fun s pos len ->
            Write.with_flow r @@ fun c ->
            Write.string c ~off:pos ~len s)
         (fun () ->
            Write.with_flow r @@ fun c ->
            Write.flush c)

    let err_formatter = formatter_of_resource stderr

    let out_formatter = formatter_of_resource stdout

    let info msg =
      Format.fprintf err_formatter
        (if testing then
          "@[%s@]@?"
        else
          "@[<hov 2>[info] %s @]@.")
        msg


    let debug fmt = Eio.traceln ("[debug] " ^^ fmt)


    let prompt_message : Protocol.local_msg_id -> unit option
      = fun (Local id) ->
        if testing then
          Option.Some ()
        else
        try
          Format.fprintf out_formatter
            "@[(%d) enter msg> @]@." id
          |> Option.some
        with
          _ -> Option.None


    let received_message : username -> Bytes.t -> unit option
      = fun username bytes ->
        try
          (if testing then
             Format.fprintf out_formatter "@[%s@]@." (String.of_bytes bytes)
           else
           Format.fprintf out_formatter "@[%s said: %s@]@." username (String.of_bytes bytes)
          )
          |> Option.some
        with
          _ -> Option.None


    let get_message : unit -> Bytes.t option
      = fun () ->
        try
          let from_terminal = Read.of_flow stdin ~initial_size:100 ~max_size:1_000_000 in
          Read.line from_terminal
          |> Bytes.of_string
          |> Option.some
        with
          _ -> Option.None


    let acknowledge_message : Protocol.local_msg_id -> Mtime.span -> unit option
      = fun (Local id) span ->
        if testing then
          Option.Some ()
        else
        try
          Format.fprintf out_formatter
            "@[msg (%d) took: %a@]@."
            id Mtime.Span.pp span
          |> Option.some
        with
          _ -> Option.None
  end : Ui)
