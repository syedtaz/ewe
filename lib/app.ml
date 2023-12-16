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
  val subscriptions : Events.Key.t -> Action.t Sub.t
end

module St = Incremental.Make ()

module Make (C : Component) = struct
  let start ~name =
    let run st =
      let open Incremental in
      let model = Var.create st (C.initial_model ()) in
      let model_w = Var.watch model in
      let update_incr =
        let open Incremental.Let_syntax in
        let%bind view = C.view model_w (fun _ -> ()) in
        return
          st
          (Runtime.Io.write_out (Term.move_cursor (1, 0) ^ Term.erasel ^ Vdom.repr view))
      in
      let _ = observe update_incr in
      stabilize st;
      let open Async in
      let eventr, eventw = Pipe.create () in
      let updatef msg =
        Var.set model (C.Action.apply msg (Var.latest_value model));
        stabilize st
      in
      Sub.start updatef C.subscriptions ~reader:eventr ~writer:eventw
    in
    let _ = Term.startup name in
    run St.State.t;
    Core.never_returns (Async.Scheduler.go ())
  ;;
end

(* V *)
