(** Start an ewe application. *)

(** Todo!
    [ ] Layout engine.
    | [ ] Get current cursor not total size.
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
  Termutils.hcursor stdout
;;

module Const = struct
  module Model = struct
    type t = string
  end

  module Action = struct
    type t = unit

    let apply _a m = m
  end

  let view model _action =
    let open Incremental.Let_syntax in
    let%map model = model in
    Vdom.text ("" ^ model) []
end

let initial_model () : Const.Model.t = "Saad"

module App = App.Run (Const)
module St = Make ()

let start () =
  let _ = startup () in
  let model = Var.create St.State.t (initial_model ()) in
  let model_w = Var.watch model in
  App.run model_w St.State.t;
  never_returns (Scheduler.go ())
;;
