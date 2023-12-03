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
  let truncframes = map nframes_w ~f:(fun x -> (x / 50) * 50)
  let truncframes_o = observe truncframes

  let stdout = force Writer.stdout
  let fticker = Dom.fticker
  let sticker = Dom.sticker

  let eff_scheduler : (int Effect.t * int) Deque.t = Effect.create ()

  let paint_f = Effect.Callback (fun y -> Dom.paint ~repaint:true stdout fticker y)
  let paint_gap = Effect.Callback (fun _ -> Writer.write stdout " < -- > ")
  let paint_g = Effect.Callback (fun y -> Dom.paint stdout sticker y)

  (** [ticker] ticks every 16.67ms and updates the [nframes] variable. *)
  let ticker () =
    let framerate = 60.0 in
    Clock.every
      (sec (1. /. framerate))
      (fun () ->
        let temp = Var.value nframes + 1 in
        Var.set nframes temp;
        stabilize St.State.t;
        Effect.schedule (paint_f, Observer.value_exn nframes_o) eff_scheduler;
        Effect.schedule (paint_gap, 0) eff_scheduler;
        Effect.schedule (paint_g, Observer.value_exn truncframes_o) eff_scheduler;
        Effect.perform_all_exn eff_scheduler)
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
