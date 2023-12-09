module type Component = sig
  module Model : sig
    type t
  end

  module Action : sig
    type t

    val apply : t -> Model.t -> Model.t
  end

  val view : Model.t -> (Action.t -> unit) -> Vdom.t
end

module type App = Component

module Run (C : Component) = struct
  open Core
  open Async

  type model = C.Model.t
  type action = C.Action.t

  let stdout = force Writer.stdout

  let run init =
    Clock.every (sec 0.5) (fun () ->
      let node = C.view init (fun _ -> ()) in
      Writer.write stdout (Vdom.repr node))
  ;;
end
