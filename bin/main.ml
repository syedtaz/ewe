open Async
open Core

let stdin_reader = force Reader.stdin
let stdout_writer = force Writer.stdout
let echo s = Writer.write stdout_writer s

let event_loop () =
  ignore (Deferred.create (fun finished ->
    let rec loop () =
      upon (Reader.read_char stdin_reader) (function
        | `Ok res -> echo (Format.sprintf "Got character %c\n" res); loop ();
        | `Eof -> Ivar.fill finished (); echo "We are done!\n")
    in loop ()
  ))

let raw_mode () =
  let open Core_unix in
  let fd = File_descr.of_int 0 in
  let stdin = Terminal_io.tcgetattr fd in
  stdin.c_icanon <- false;
  stdin.c_echo <- false;
  Terminal_io.tcsetattr stdin fd ~mode:Terminal_io.TCSANOW

let () =
  raw_mode ();
  event_loop ();
  never_returns (Scheduler.go ())
