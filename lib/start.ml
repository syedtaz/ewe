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

  type 'a t =
    { nframes : (int, 'a) Var.t
    ; nframes_o : (int, 'a) Observer.t
    ; truncframes_o : (int, 'a) Observer.t }

  (** [init_incr] will initialize a variable to keep track of the frame rate alongside the
      incremental nodes that depend on the frame rate. *)
  let init_incr state : 'a t =
    let nframes = Var.create state 0 in
    let nframes_w = Var.watch nframes in
    let nframes_o = observe nframes_w in
    let truncframes = map nframes_w ~f:(fun x -> x / 50 * 50) in
    let truncframes_o = observe truncframes in
    { nframes = nframes; nframes_o = nframes_o; truncframes_o = truncframes_o}
  ;;

  (* Components *)
  let stdout = force Writer.stdout

  let fticker = Dom.fticker
  and sticker = Dom.sticker

  (** [paint_f] and [paint_g] are helper functions for writing to stdout. *)
  let paint_f { nframes_o = obs; _ } =
    Effect.Fun
      (fun () ->
        Dom.paint ~repaint:true stdout fticker (Observer.value_exn obs);
        Writer.write stdout " < -- > ")
  ;;

  let paint_g { truncframes_o = obs; _ } =
    Effect.Fun (fun () -> Dom.paint stdout sticker (Observer.value_exn obs))
  ;;

  let ticker frame_t queue =
    let framerate = 60.0 in
    Async.Clock.every
      (sec (1. /. framerate))
      (fun () ->
        let temp = Incremental.Var.value frame_t.nframes + 1 in
        Incremental.Var.set frame_t.nframes temp;
        Queue.schedule (paint_f frame_t) queue;
        Queue.schedule (paint_g frame_t) queue;)
end

(** [on_startup] sets the state of the terminal at the beginning of the app.
    This includes hiding the cursor and disabling canonincal and echo mode.
    Takes an arbitrary function [f] that can be used to schedule user defined
    (possibly side-effecting) functions before the app begins properly.*)
let on_startup f =
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

module St = Make ()

let start () =
  let (frame_t, queue) = on_startup (fun () -> (Frames.init_incr St.State.t, Effect.Queue.create ())) in
  Effect.Queue.ticker St.State.t queue;
  Frames.ticker frame_t queue;
  never_returns (Scheduler.go ())
;;
