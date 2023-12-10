open Core

type tag = ..
type tag += Node | Text

type t = Element of element

and element =
  { id : string
  ; tag : tag
  ; value : string option
  ; grid : Grid.t
  ; children : t list
  }

let text ~id body ~grid children =
  Element { id; tag = Text; value = Some body; grid; children }
;;

let node ~id ~grid children = Element { id; tag = Node; value = None; grid; children }

let repr node =
  let rec aux acc n =
    match n with
    | Element { value = v; children = ch; _ } ->
      let v' = Option.value v ~default:"" in
      let rest = List.fold ch ~init:acc ~f:aux in
      rest ^ v'
  in
  aux "" node
;;
