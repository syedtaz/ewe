module type T = sig
  module Model : sig
    type t
  end

  module Action : sig
    type t

    val apply : t -> Model.t -> Model.t
  end

  (* module Sub : Sub.Sub *)

  val view
    :  (Model.t, 'a) Incremental.t
    -> (Action.t -> unit)
    -> (Vdom.t, 'a) Incremental.t

  val initial_model : unit -> Model.t
  val subscriptions : Events.Key.t -> Action.t Sub.t
end
