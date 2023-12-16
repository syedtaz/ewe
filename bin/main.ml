open Ewe
module App = App.Make (Examples.Counter)

let () = App.start ~name:(Some "Counter")
