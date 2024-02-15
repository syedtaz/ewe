module Sub = struct
  open! Core
  open! Async

  type 'a t =
    | Msg of 'a
    | Quit
    | Nil

  let interleave rs =
    let totalr, totalw = Pipe.create () in
    let () =
      Deferred.List.iter ~how:`Parallel rs ~f:(fun x -> Pipe.transfer_id x totalw)
      >>> fun () -> Pipe.close totalw
    in
    totalr
  ;;

  type 'a key_decoder = Events.Key.t -> 'a t
  type 'a signal_decoder = Signal.t list * (Signal.t -> 'a t)
  type 'a decoders = 'a key_decoder * 'a signal_decoder

  let rec start ~(decoder : 'a decoders) ~update =
    let key_decoder, (signals, signal_decoder) = decoder in
    let keyr, keyw = Pipe.create () in
    let signalr, signalw = Pipe.create () in
    let () = don't_wait_for (keypress_aux key_decoder keyw) in
    let () = signal_aux signals signal_decoder signalw in
    let () = don't_wait_for (schedule (interleave [ keyr; signalr ]) update) in
    ()

  and keypress_aux f keyw =
    let%bind input = Reader.read_char Runtime.Io.stdin in
    match input with
    | `Eof -> keypress_aux f keyw
    | `Ok c ->
      let c' = Events.Key.lift c in
      (match f c' with
       | Msg v ->
         let () = ignore (Pipe.write keyw v) in
         keypress_aux f keyw
       | Nil -> keypress_aux f keyw
       | Quit -> Term.shutdown ())

  and signal_aux signals signal_decoder signalw =
    let handler signal =
      match signal_decoder signal with
      | Msg v -> ignore (Pipe.write signalw v)
      | Nil -> ()
      | Quit -> Term.shutdown ()
    in
    Signal.handle signals ~f:handler

  and schedule (reader : 'a Pipe.Reader.t) update =
    let%bind result = Pipe.read' reader in
    match result with
    | `Eof -> schedule reader update
    | `Ok msgs ->
      let () = Queue.iter ~f:(fun x -> update x) msgs in
      schedule reader update
  ;;
end

include Sub