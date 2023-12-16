(** Start an ewe application. *)

(** Todo!
    [ ] Layout engine.
    [ ] Simple VDom diffing. *)

open! Core
open! Async
open! Incremental

(** [on_startup] sets the state of the terminal at the beginning of the app.
    This includes hiding the cursor and disabling canonincal and echo mode.
    Takes an arbitrary function [f] that can be used to schedule user defined
    (possibly side-effecting) functions before the app begins properly.*)
let startup ~name =
  let open Core_unix in
  let fd = File_descr.of_int 0 in
  let stdin = Terminal_io.tcgetattr fd in
  stdin.c_icanon <- false;
  stdin.c_echo <- false;
  Terminal_io.tcsetattr stdin fd ~mode:TCSANOW;
  Termutils.hcursor ();
  Termutils.erase_screen ();
  match name with
  | Some v -> Termutils.Window.save_and_set v
  | None -> ()
;;

(* let shutdown () =
   let open Core_unix in
   let fd = File_descr.of_int 0 in
   let stdin = Terminal_io.tcgetattr fd in
   stdin.c_icanon <- true;
   stdin.c_echo <- true;
   Terminal_io.tcsetattr stdin fd ~mode:TCSANOW;
   Termutils.scursor ();
   exit 0
   ;; *)

module St = Make ()

let start ~name f =
  let _ = startup ~name in
  f St.State.t;
  never_returns (Scheduler.go ())
;;
