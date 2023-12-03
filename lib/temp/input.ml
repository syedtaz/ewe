(* open Bonsai
open Core
open Async

module IState = struct
  type t =
    | Empty
    | Text of char
    | Eof
  [@@deriving sexp, equal]

  let to_string = function
    | Empty -> ""
    | Text c -> Format.sprintf "\x1b[1D%c" c
    | Eof -> "\x1b[1DWe are done"
  ;;
end

open Bonsai.Let_syntax

let stdin_reader = force Reader.stdin

let ping =
  let cur = Var.create IState.Empty in
  upon (Reader.read_char stdin_reader) (function
    | `Ok res -> if Char.equal res '\x04' then Var.set cur Eof else Var.set cur (Text res)
    | `Eof -> Var.set cur Eof);
  Var.value cur
;;

let view =
  let%arr c = ping in
  "Got the character:  " ^ IState.to_string c
;;

(* let keypress = function
   | flush when Char.equal flush '\x04' -> IState.Eof
   | res -> IState.Text res

   let keypress_e = Bonsai_web.Effect.of_sync_fun keypress

   let on_keypress x f : unit Effect.t =
   let%bind.Effect s = keypress_e x in
   f s *)
(*
   let view =
   let ichar = Bonsai.state (module IState) ~default_model:Empty in
   let up_update = sub ichar ~f:(fun x -> pure (fun (_, f) -> on_keypress 'x' f) x) in
*)

(* let event_loop () =
   let handler finished =
   let rec loop () =
   upon (Reader.read_char stdin_reader) (function
   | `Ok value ->
   if Char.equal value '\x04' then (
   Ivar.fill finished ();
   Var.set ichar value)
   else (
   Var.set ichar value;
   loop ())
   | `Eof -> Ivar.fill finished ())
   in
   loop ()
   in
   ignore (Deferred.create handler) *) *)
