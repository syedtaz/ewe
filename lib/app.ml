module type Component = sig
  module Model : sig
    type t
  end

  module Action : sig
    type t

    val apply : t -> Model.t -> Model.t
  end

  val view
    :  (Model.t, 'a) Incremental.t
    -> (Action.t -> unit)
    -> (Vdom.t, 'a) Incremental.t

  val initial_model : unit -> Model.t
  val temp_subchars : char list
  val subscriptions : Events.Signals.World.key -> Action.t Sub.t
end

module Run (C : Component) = struct
  open Core

  let run st =
    let open Incremental in
    let model = Var.create st (C.initial_model ()) in
    let model_w = Var.watch model in
    let update_incr =
      C.view model_w (fun _ -> ())
      >>= fun x ->
      return
        st
        (Runtime.Io.write_out
           (Termutils.move_cursor (1, 0) ^ Termutils.erasel ^ Vdom.repr x))
    in
    let _ = observe update_incr in
    stabilize st;
    let open Async in (
    let eventr, eventw = Pipe.create () in
    Sub.on_keypress C.subscriptions C.temp_subchars eventw;
    let rec loop () =
    Pipe.read' eventr >>= fun x -> match x with
      | `Eof -> loop ()
      | `Ok msg -> (
        let apply event = match event with
          | Sub.Cmd msg -> Var.set model (C.Action.apply msg (Var.latest_value model))
          | Sub.Nil -> ()
      in
      Queue.iter msg ~f:apply; loop ())
    in ignore (loop ()))
  ;;
end
