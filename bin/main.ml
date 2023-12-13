open Ewe
module App = App.Run (Examples.Counter)

let () = Start.start ~name:(Some "Counter") App.run
