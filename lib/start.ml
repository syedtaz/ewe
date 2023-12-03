(** Start an ewe application. *)

(** Goals for this implementation:
    [ ] Loop forever until SIGKILL.
    [ ] Track frames and aim for 60 fps. *)

open! Core
open! Async

module Termutils = struct
  let hcursor writer = Writer.write writer "\x1b[?25l"
end

(** [on_startup] sets the state of the terminal at the beginning of the app. *)
let on_startup () =
  let open Core_unix in
  let stdout_writer = force Writer.stdout in
  let fd = File_descr.of_int 0 in
  let stdin = Terminal_io.tcgetattr fd in
  stdin.c_icanon <- false; stdin.c_echo <- false;
  Terminal_io.tcsetattr stdin fd ~mode:TCSANOW;
  Termutils.hcursor stdout_writer

let start () =
  on_startup ();
  never_returns (Scheduler.go ())