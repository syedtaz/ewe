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
  open! Async
  open! Incremental

  let tick model =
    let open Async in
    Clock.every (sec 0.5) (fun () -> Vdom.print model)
  (* let open Text in
     let model = Var.create st (Model.of_tuple (Termutils.tsize ())) in
     let model_w = Var.watch model in
     let model_v = view model_w in
     let model_o = observe model_v in
     let sigwinch = Signal.of_caml_int 28 in
     Signal.handle [ sigwinch ] ~f:(fun _ ->
     Incremental.Var.set model (Model.of_tuple (Action.apply `SIGWINCH));
     stabilize st;
     Writer.write stdout (Observer.value_exn model_o)) *)
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
  Termutils.hcursor stdout
;;

module St = Make ()

let start () =
  let _ = startup () in
  let model = Vdom.text (Some "Hello world!") [] in
  Frames.tick model;
  never_returns (Scheduler.go ())
;;
