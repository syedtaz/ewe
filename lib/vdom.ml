open Core

type t = Element of element

and element =
  { tag : string
  ; value : string option
  ; children : t list
  }

let text body children = Element { tag = "text"; value = Some body; children }

let repr node =
  let rec aux acc n = match n with
    | Element { value = v; children = ch; _ } ->
      let v' = Option.value v ~default:"" in
      let rest = List.fold ch ~init:acc ~f:(aux) in
      v' ^ rest
    in aux "" node
