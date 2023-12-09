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
end

module type App = Component

module Run (C : Component) = struct
  open Core
  open Async

  type model = C.Model.t
  type action = C.Action.t

  let stdout = force Writer.stdout

  let run node st =
    let open Incremental in
    let x = C.view node (fun _ -> ()) >>= fun x -> return st (Writer.write stdout (Vdom.repr x)) in
    let _ = observe x in
    stabilize st;
  ;;
end
