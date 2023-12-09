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
    let _, cols = Termutils.tsize () in
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
          Map.set rest ~key:t ~data:(nr, nc'))
        else (
          let rest = List.fold ch ~init:acc ~f:(fun a v -> aux a (cr, nc) v) in
          Map.set rest ~key:t ~data:(cr, nc))
    in
    aux (M.empty) (1, 0) node
  ;;
end

include Layout
