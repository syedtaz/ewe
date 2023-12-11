module Vdom = struct
  open Core

  type tag = ..
  type tag += Node | Text

  include Grid

  type t = Element of element

  and element =
    { id : string
    ; tag : tag
    ; value : string option
    ; grid : Grid.t
    ; children : t list
    }
  [@@deriving fields]

  let text ~id ~grid body children =
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

  let deject = function
    | Element e -> e
  ;;

  let fold (node : t) (acc : 'a) ~(f : 'a -> string option -> 'a) =
    match deject node with
    | { value; children; _ } ->
      if List.length children = 0
      then f acc value
      else (
        let temp =
          List.fold_left children ~init:acc ~f:(fun acc x -> f acc (deject x).value)
        in
        f temp value)
  ;;
end

module Tests = struct
  open Core
  open Vdom

  let%test_unit "simple_fold" =
    let f acc sopt = acc ^ Option.value sopt ~default:"" in
    let n = text ~id:"somenode" "" ~grid:(colidx 0) [] in
    let result = fold n "" ~f in
    [%test_eq: string] "" result
  ;;

  let%test_unit "multi_fold" =
    let f acc sopt = acc ^ Option.value sopt ~default:"" in
    let n =
      text
        ~id:"somenode"
        "hello"
        ~grid:(colidx 0)
        [ text ~id:"internalnode" "world" ~grid:(colidx 0) []
        ; text ~id:"internalnode" "world" ~grid:(colidx 0) []
        ]
    in
    let result = fold n "" ~f in
    [%test_eq: string] "worldworldhello" result
  ;;
end

include Vdom
