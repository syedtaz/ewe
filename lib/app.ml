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
  val mapping : Keyboard.key -> Action.t Option.t
end

module ActionQueue (C : Component) = struct
  open Core
  open Incremental

  type eff = C.Action.t
  and t = eff Deque.t

  let create () : t = Deque.create ~initial_length:10 ()

  let run (queue : t) (model : (C.Model.t, 'a) Var.t) model_o st =
    let open Async in
    let framerate = 1. /. 60. in
    Clock.every (sec framerate) (fun () ->
      let length = Deque.length queue in
      List.iter
        (List.init length ~f:(fun _ -> ()))
        ~f:(fun _ ->
          let eff = Deque.dequeue_front_exn queue in
          let model' = C.Action.apply eff (Observer.value_exn model_o) in
          Var.set model model';
          stabilize st))
  ;;
end

module Run (C : Component) = struct
  open Core
  open Async
  module Q = ActionQueue (C)

  let run st =
    let open Incremental in
    let stdout = force Writer.stdout in
    let model = Var.create st (C.initial_model ()) in
    let model_w = Var.watch model in
    let model_o = observe model_w in
    let x =
      C.view model_w (fun _ -> ())
      >>= fun x ->
      return
        st
        (Writer.write
           stdout
           (Termutils.move_cursor (2, 0) ^ Termutils.erasel ^ Vdom.repr x))
    in
    let _ = observe x in
    stabilize st;
    let queue = Q.create () in
    Q.run queue model model_o st;
    ignore (Keyboard.read C.mapping queue)
  ;;
end
