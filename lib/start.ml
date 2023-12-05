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

module Frames = struct
  open Async
  open Incremental
  open Incremental.Let_syntax

  let tick st =
    (* Define incrementals. *)
    let nframes = Var.create st 0 in
    let nframes_w = Var.watch nframes in
    let trunc_t =
      let%map nframes = nframes_w in
      nframes / 50 * 50
    in
    (* Define effectful incrementals. *)
    let nframes_eff =
      let%map x = nframes_w in
      Incremental.return
        st
        (Writer.write
           stdout
           (Termutils.move_cursor (0, 0)
            ^ Format.sprintf "Current frame: %d < ~ > " x))
    in
    let _ = observe nframes_eff in
    let trunc_eff =
      let%map x = trunc_t in
      Incremental.return st (Writer.write stdout (Format.sprintf "%d;" x))
    in
    let _ = observe trunc_eff in
    (* Define ticker. *)
    let framerate = 60. in
    Async.Clock.every
      (sec (1. /. framerate))
      (fun () ->
        let temp = Incremental.Var.value nframes + 1 in
        Incremental.Var.set nframes temp;
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
  Effect.Queue.create ()
;;

module St = Make ()

let start () =
  let _ = startup () in
  Frames.tick St.State.t;
  never_returns (Scheduler.go ())
;;
