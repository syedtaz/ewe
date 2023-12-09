open Async
open Core

type t = Element of element

and element =
  { tag : string
  ; value : string option
  ; children : t list
  }

let text body children = Element { tag = "text"; value = body; children }

let stdout = force Writer.stdout

let rec print = function
  | Element {tag = _; value = v; children = ch} ->
    let () = match v with
    | Some s -> Writer.write stdout s
    | None -> ()
    in List.iter ch ~f:(fun x -> print x)
