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
end

module type App = Component

module Run (C : Component) = struct
  open Core
  open Async

  type model = C.Model.t
  type action = C.Action.t

  let stdout = force Writer.stdout

  let run st =
    let open Incremental in
    let model = Var.create st (C.initial_model ()) in
    let model_w = Var.watch model in
    let x = C.view model_w (fun _ -> ()) >>= fun x -> return st (Writer.write stdout (Vdom.repr x)) in
    let _ = observe x in
    stabilize st;
  ;;
end
