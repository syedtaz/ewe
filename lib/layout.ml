module Layout = struct
  open Core

  let stdout =
    let open Async in
    force Writer.stdout
  ;;

  type cursor = int * int

  type position =
    { start : cursor
    ; fin : cursor
    }

  let write cursor payload =
    let open Async in
    let out = Termutils.move_cursor cursor ^ payload in
    Writer.write stdout out
  ;;

  let size node = Vdom.repr node |> String.length
  let rows, cols = Termutils.tsize ()

  let split (crow, ccol) size str =
    let rec aux (crow, ccol) acc str =
      let len = String.length str in
      if len > size
      then (
        let chunk = String.sub ~pos:0 ~len:size str ^ "\n" in
        let rest = String.sub ~pos:size ~len:(len - size) str in
        aux (crow + 1, ccol) (((crow, ccol), chunk) :: acc) rest)
      else ((crow, ccol), str) :: acc
    in
    List.rev (aux (crow, ccol) [] str)
  ;;
end

module Tests = struct
  open Core
  open Layout

  type t = ((int * int) * string) [@@deriving compare, sexp]

  let%test_unit "split_no_split" =
    [%test_eq: t list] [((1, 0), "hello")] (split (1, 0) 10 "hello")

  let%test_unit "split_one_split" =
  [%test_eq: t list] [((1, 0), "hello\n"); ((2, 0), "world")] (split (1, 0) 5 "helloworld")

  (* let%test "split_one_split" =
    List.equal compare [((1, 0), "hello"); ((2, 0), "world")] (split (1, 0) 5 "helloworld") *)
end

include Layout
