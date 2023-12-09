open Ewe

module Const = struct
  module Model = struct
    type t = string
  end

  module Action = struct
    type t = unit

    let apply _a m = m
  end

  let view model _action =
    let open Incremental.Let_syntax in
    let%map model = model in
    Vdom.text ("" ^ model) []

  let initial_model () = "Hello world."
end

module App = App.Run(Const)

let () = Start.start App.run
