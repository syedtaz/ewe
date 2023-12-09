open Ewe

module Counter = struct
  module Model = struct
    type t = int
  end

  module Action = struct
    type t =
      | Increment
      | Decrement

    let lift x =
      match x with
      | "Increment" -> Increment
      | "Decrement" -> Decrement
      | _ -> raise (Invalid_argument "Invalid signal")
    ;;

    let apply action m =
      match action with
      | Increment -> m + 1
      | Decrement -> m - 1
    ;;
  end

  let view model _action =
    let open Incremental.Let_syntax in
    let%map model = model in
    Vdom.text (string_of_int model) []
  ;;

  let initial_model () = 1

  let mapping (key : Keyboard.key) =
    let open Action in
    match key with
    | A -> Some Increment
    | B -> Some Decrement
    | _ -> None
end

module App = App.Run (Counter)

let () = Start.start App.run
