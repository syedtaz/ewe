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

  let view model _action = Vdom.text ("" ^ model) []
end

let initial_model () : Const.Model.t = "Saad"

module App = App.Run (Const)

let reader = force Reader.stdin


let start () =
  let _ = startup () in
  let res = Layout.map (Const.view (initial_model ()) ()) in
  List.iter ~f:(fun (s, (y, x)) -> Writer.write stdout (Format.sprintf "Tag: %s at (%d, %d)" s y x)) res;
  never_returns (Scheduler.go ())
;;
