(** Start an ewe application. *)

(** Goals for this implementation:
    [x] Loop forever until SIGKILL.
    [x] Track frames and aim for 60 fps.
    [ ] Effect queue on separate loop.
    [ ] Simple VDom diffing. *)

open! Core
open! Async
open! Incremental

module Frames = struct
  open Async
  open Effect
  module St = Make ()

  type obs = (int, St.state_witness) Observer.t

  (** [init_incr] will initialize a variable to keep track of the frame rate alongside the
      incremental nodes that depend on the frame rate. *)
  let init_incr () =
    let nframes = Var.create St.State.t 0 in
    let nframes_w = Var.watch nframes in
    let nframes_o = observe nframes_w in
    let truncframes = map nframes_w ~f:(fun x -> x / 50 * 50) in
    let truncframes_o = observe truncframes in
    nframes, nframes_o, truncframes_o
  ;;

  (* Components *)
  let stdout = force Writer.stdout

  let fticker = Dom.fticker
  and sticker = Dom.sticker

  (** [eff_scheduler] is queue that keeps track of effectful functions. *)
  let eff_scheduler = EffectQueue.create St.State.t

  (** [paint_f] and [paint_g] are helper functions for writing to stdout. *)
  let paint_f =
    Effect.Fun
      (fun y ->
        Dom.paint ~repaint:true stdout fticker (Observer.value_exn y);
        Writer.write stdout " < -- > ")

  and paint_g = Effect.Fun (fun y -> Dom.paint stdout sticker (Observer.value_exn y))

  (** [ticker] ticks every 16.67ms and updates the [nframes] variable. *)
  let ticker () =
    let nframes, nframes_o, truncframes_o = init_incr () in
    let framerate = 60.0 in
    Clock.every
      (sec (1. /. framerate))
      (fun () ->
        let temp = Var.value nframes + 1 in
        Var.set nframes temp;
        EffectQueue.schedule (paint_f, nframes_o) St.State.t eff_scheduler;
        EffectQueue.schedule (paint_g, truncframes_o) St.State.t eff_scheduler;
        EffectQueue.perform_all_exn St.State.t eff_scheduler)
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
