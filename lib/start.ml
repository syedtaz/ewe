(** Start an ewe application. *)

(** Goals for this implementation:
    [x] Loop forever until SIGKILL.
    [x] Track frames and aim for 60 fps.
    [x] Effect queue on separate loop.
    [x] Effectful incrementals
    [ ] Simple VDom diffing. *)

open! Core
open! Async
open! Incremental

let stdout = force Writer.stdout

external tsize : unit -> (int * int) = "tsize"

module Frames = struct
  open Async
  open Incremental
  open Incremental.Let_syntax

  let tick st =
    (* Define incrementals. *)

    let (r_init, c_init) = tsize () in
    let termsize = Var.create st (r_init, c_init) in
    let termsize_w = Var.watch termsize in
    let termsize_eff =
      let%map (r, c) = termsize_w in
      Incremental.return st (Writer.write stdout (Termutils.move_cursor (0, 0) ^ Termutils.erasel ^ Format.sprintf "(number of rows %d and number of cols %d)" r c))
    in
    let _ = observe termsize_eff in
    let framerate = 60. in
    Async.Clock.every (sec (1. /. framerate)) (fun () ->
      let size = tsize () in
      Incremental.Var.set termsize size;
      stabilize st)
    ;;
end

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
;;

module St = Make ()

let start () =
  let _ = startup () in
  Frames.tick St.State.t;
  never_returns (Scheduler.go ())
;;
