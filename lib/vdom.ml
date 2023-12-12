module Vdom = struct
  open Core

  type tag = ..
  type tag += Node | Text

  include Grid

  type t = Element of element

  and element =
    { id : string
    ; tag : tag
    ; attrs : attr list
    ; value : string option
    ; grid : Grid.t
    ; children : t list
    }
  [@@deriving fields]

  and attr = ..

  module Attributes = struct
    type color = RGB of int * int * int
    type attr += Color of color

    let rgb red green blue = Color (RGB (red, green, blue))

    let apply1 attr value =
      match attr with
      | Color (RGB (r, g, b)) ->
        Format.sprintf "%s%s%s" (Termutils.rgb r g b) value Termutils.clearfmt
      | _ -> raise (Invalid_argument "Unknown variant")
    ;;

    let apply attrs value =
      List.fold_left ~init:value ~f:(fun acc attr -> apply1 attr acc) attrs
    ;;
  end

  let text ~id ~grid ~attrs body children =
    Element { id; tag = Text; attrs; value = Some body; grid; children }
  ;;

  let node ~id ~grid ~attrs children =
    Element { id; tag = Node; attrs; value = None; grid; children }
  ;;

  let repr node =
    let rec aux acc n =
      match n with
      | Element { value = v; attrs; children = ch; _ } ->
        let v' = Attributes.apply attrs (Option.value v ~default:"") in
        let rest = List.fold ch ~init:acc ~f:aux in
        rest ^ v'
    in
    aux "" node
  ;;

  let deject = function
    | Element e -> e
  ;;

  (** TODO! Maybe refactor this? *)
  let fold_value (node : t) (acc : 'a) ~(f : 'a -> string option -> 'a) =
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
    let n = text ~id:"somenode" "" ~grid:(colidx 0) ~attrs:[] [] in
    let result = fold_value n "" ~f in
    [%test_eq: string] "" result
  ;;

  let%test_unit "multi_fold" =
    let f acc sopt = acc ^ Option.value sopt ~default:"" in
    let n =
      text
        ~id:"somenode"
        ~attrs:[]
        "hello"
        ~grid:(colidx 0)
        [ text ~id:"internalnode" "world" ~attrs:[] ~grid:(colidx 0) []
        ; text ~id:"internalnode" "world" ~attrs:[] ~grid:(colidx 0) []
        ]
    in
    let result = fold_value n "" ~f in
    [%test_eq: string] "worldworldhello" result
  ;;
end

include Vdom
