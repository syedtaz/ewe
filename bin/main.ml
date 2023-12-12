open Ewe

module M = struct
  module Model = struct
    type t = unit
  end

  module Action = struct
    type t = unit

    let apply _action m = m
  end

  let view model _action =
    let open Incremental.Let_syntax in
    let open Vdom in
    let%map _model = model in
    text
      ~id:"root"
      ~attrs:[ Attributes.rgb 255 192 203 ]
      ~grid:(colidx2 0 0)
      "CSS in the terminal. We have peaked!"
      []
  ;;

  let initial_model () = ()

  let mapping = function
    | _ -> None
  ;;
end

module App = App.Run (M)

let () = Start.start App.run
