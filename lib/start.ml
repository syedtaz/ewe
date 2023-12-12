(** Start an ewe application. *)

(** Todo!
    [ ] Layout engine.
    [ ] Simple VDom diffing. *)

open! Core
open! Async
open! Incremental

let stdout = force Writer.stdout

(** [on_startup] sets the state of the terminal at the beginning of the app.
    This includes hiding the cursor and disabling canonincal and echo mode.
    Takes an arbitrary function [f] that can be used to schedule user defined
    (possibly side-effecting) functions before the app begins properly.*)
let startup () =
  let open Core_unix in
  let stdout = force Writer.stdout in
  let fd = File_descr.of_int 0 in
  let stdin = Terminal_io.tcgetattr fd in
  stdin.c_icanon <- false;
  stdin.c_echo <- false;
  Terminal_io.tcsetattr stdin fd ~mode:TCSANOW;
  Termutils.hcursor stdout;
  Termutils.erase_screen stdout
;;

let shutdown () =
  let open Core_unix in
  let stdout = force Writer.stdout in
  let fd = File_descr.of_int 0 in
  let stdin = Terminal_io.tcgetattr fd in
  stdin.c_icanon <- true;
  stdin.c_echo <- true;
  Terminal_io.tcsetattr stdin fd ~mode:TCSANOW;
  Termutils.scursor stdout;
  exit 0
;;

module St = Make ()

let start f =
  let _ = startup () in
  f St.State.t;
  never_returns (Scheduler.go ())
;;
