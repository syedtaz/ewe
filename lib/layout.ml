open Vdom

module Layout = struct
  open Core
  module M = Map.Make (String)

  type t = position M.t

  and position =
    { start : int
    ; fin : int
    }

  let map node =
    let rows, cols = Termutils.tsize () in
    let rec aux acc (cr, cc) n =
      match n with
      | Element { tag = t; value = v; children = ch; _ } ->
        let l = String.length (Option.value ~default:"" v) in
        let nc = cc + l in
        if nc >= cols
        then (
          let nc' = nc mod cc in
          let nr = cr + 1 in
          let rest = List.fold ch ~init:acc ~f:(fun a v -> aux a (nr, nc') v) in
          (t, (nr, nc')) :: rest)
        else (
          let rest = List.fold ch ~init:acc ~f:(fun a v -> aux a (cr, nc) v) in
          (t, (cr, nc)) :: rest)
    in
    aux [] (rows, cols) node
  ;;
end

include Layout
