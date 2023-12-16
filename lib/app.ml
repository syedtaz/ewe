module St = Incremental.Make ()

module Make (C : Component.T) = struct
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
      let update msg =
        let () = Var.set model (C.Action.apply msg (Var.latest_value model)) in
        stabilize st
      in
      Sub.start ~decoder:C.subscriptions ~update
    in
    let _ = Term.startup name in
    run St.State.t;
    Core.never_returns (Async.Scheduler.go ())
  ;;
end