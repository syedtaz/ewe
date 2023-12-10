open Ewe

module Counter = struct
  open Incremental
  open Incremental.Let_syntax

  module Model = struct
    type t =
      { counter : int
      ; label : string
      }
    [@@deriving fields]
  end

  module Action = struct
    open Model

    type t =
      | Increment
      | Decrement
      | FlipName

    let apply action { counter = c; label = l } =
      match action with
      | Increment -> { counter = c + 1; label = l }
      | Decrement -> { counter = c - 1; label = l }
      | FlipName ->
        if l = "Ross"
        then { counter = c; label = "Taz" }
        else { counter = c; label = "Ross" }
    ;;
  end

  let view model _action =
    let open Grid in
    (* Define variables track the value of label and count. *)
    let label = map model ~f:Model.label in
    let count = map model ~f:Model.counter in
    (* Extract the current value of label and count. *)
    let%map label = label
    and count = count in
    Vdom.node
      ~id:"counterComponent"
      ~grid:(ColDef [ Px 100; Px 100 ])
      [ Vdom.node
          ~id:"counterComponent"
          ~grid:(ColIdx (0, 1))
          [ Vdom.text
              ~id:"textComponent"
              ~grid:(ColIdx (0, 0))
              (Format.sprintf "%s is sleepy" label)
              []
          ; Vdom.text
              ~id:"intCountComponent"
              ~grid:(ColIdx (1, 1))
              (Format.sprintf "and the counter is %d" count)
              []
          ]
      ]
  ;;

  let initial_model () : Model.t = { counter = 0; label = "Ross" }

  let mapping (key : Keyboard.key) =
    let open Action in
    match key with
    | A -> Some Increment
    | B -> Some Decrement
    | C -> Some FlipName
    | _ -> None
  ;;
end

module App = App.Run (Counter)

let () = Start.start App.run
