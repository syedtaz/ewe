(* open Ewe

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
   let%map model = model in
   Vdom.text (Format.sprintf "Counter -> %d" model) []
   ;;

   let initial_model () = 1

   let mapping (key : Keyboard.key) =
   let open Action in
   match key with
   | A -> Some Increment
   | B -> Some Decrement
   | _ -> None
   end *)
