open Ewe

module Counter = struct
  module Model = struct
    type t = int
  end

  module Action = struct
    type t =
      | Increment
      | Decrement

    let apply action m =
      match action with
      | Increment -> m + 1
      | Decrement -> m - 1
    ;;
  end

  let view model _action =
    let open Incremental.Let_syntax in
    let open Vdom in
    let%map model = model in
    node
      ~id:"CounterW"
      ~grid:(coldef [ px 200 ])
      [ text ~id:"Counter" ~grid:(colidx2 0 0) (Format.sprintf "Counter -> %d" model) [] ]
  ;;

  let initial_model () = 1

  let mapping (key : Keyboard.key) =
    let open Action in
    match key with
    | A -> Some Increment
    | B -> Some Decrement
    | _ -> None
  ;;
end
