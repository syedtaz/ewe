module type Component = sig
  module Model : sig
    type t
  end

  module Action : sig
    type t

    val lift : string -> t
    val apply : t -> Model.t -> Model.t
  end

  val view
    :  (Model.t, 'a) Incremental.t
    -> (Action.t -> unit)
    -> (Vdom.t, 'a) Incremental.t

  val initial_model : unit -> Model.t
end

module Run (C : Component) = struct
  open Core
  open Async

  let mapping (x : Keyboard.key) = match x with
    | A -> "a -> "
    | B -> "b -> "
    | _ -> "not a or b -> "

  let run st =
    let open Incremental in
    let stdout = force Writer.stdout in
    let model = Var.create st (C.initial_model ()) in
    let model_w = Var.watch model in
    (* let model_o = observe model_w in *)
    let x =
      C.view model_w (fun _ -> ())
      >>= fun x -> return st (Writer.write stdout (Termutils.erasel ^ Vdom.repr x))
    in
    let _ = observe x in
    stabilize st;
    let queue = Deque.create () in
    let _ = Keyboard.read mapping queue in
    ()
  ;;
end
