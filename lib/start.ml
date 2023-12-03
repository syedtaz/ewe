(** Start an ewe application. *)

(** Goals for this implementation:
    [x] Loop forever until SIGKILL.
    [x] Track frames and aim for 60 fps.
    [ ] Simple VDom diffing. *)

open! Core
open! Async
open! Incremental

module Frames = struct
  open Async
  module St = Make ()

  (** [nframes] and [nframes_w] form an incremental variable that holds the number of frames. *)
  let nframes = Var.create St.State.t 0

  let nframes_w = Var.watch nframes
  let nframes_o = observe nframes_w
  let count_rem = map nframes_w ~f:(fun x -> x / 50)
  let count_rem_o = observe count_rem
  let stdout = force Writer.stdout
  let fticker = Dom.fticker
  let sticker = Dom.sticker

  (** [update_view] prints the stabilized value of [nframes] to standard output. *)
  let update_view () =
    Dom.paint ~repaint:true stdout fticker (Var.value nframes);
    Writer.write stdout " < -- > ";
    Dom.paint stdout sticker (Var.value nframes)
  ;;

  (** [ticker] ticks every 16.67ms and updates the [nframes] variable. *)
  let ticker () =
    let framerate = 60.0 in
    Clock.every
      (sec (1. /. framerate))
      (fun () ->
        let temp = Var.value nframes + 1 in
        Var.set nframes temp;
        stabilize St.State.t;
        update_view ())
  ;;
end

(** [on_startup] sets the state of the terminal at the beginning of the app.
    This includes hiding the cursor and disabling canonincal and echo mode.
    Takes an arbitrary function [f] that can be used to schedule user defined
    (possibly side-effecting) functions before the app begins properly.*)
let on_startup f : unit =
  let open Core_unix in
  let stdout = force Writer.stdout in
  let fd = File_descr.of_int 0 in
  let stdin = Terminal_io.tcgetattr fd in
  stdin.c_icanon <- false;
  stdin.c_echo <- false;
  Terminal_io.tcsetattr stdin fd ~mode:TCSANOW;
  Termutils.hcursor stdout;
  f ()
;;

let start () =
  on_startup Termutils.noop;
  Frames.ticker ();
  never_returns (Scheduler.go ())
;;
