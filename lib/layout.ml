module Layout = struct
  open Core
  open Async

  type t = (int * int) * string [@@deriving compare, sexp]

  let stdout =
    let open Async in
    force Writer.stdout
  ;;

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

  let paint (els : t list) =
    List.iter els ~f:(fun (cursor, payload) ->
      Writer.write stdout (Termutils.move_cursor cursor ^ payload))
  ;;

  (* let generate (n : Vdom.element) : t list =
    let size = snd (Termutils.tsize ()) in
    let rec aux acc (n : Vdom.element) : t list = match n with
      | { value; grid; children; _} -> match value with
        | Some v -> split (1, 0) size v
        | None ->
    in aux [] n *)
end

module Tests = struct
  open Core

  let%test_unit "split_no_split" =
    [%test_eq: Layout.t list] [ (1, 0), "hello" ] (Layout.split (1, 0) 10 "hello")
  ;;

  let%test_unit "split_one_split" =
    [%test_eq: Layout.t list]
      [ (1, 0), "hello\n"; (2, 0), "world" ]
      (Layout.split (1, 0) 5 "helloworld")
  ;;

  let%test_unit "split_multi_split" =
    [%test_eq: Layout.t list]
      [ (1, 0), "hello\n"; (2, 0), "world\n"; (3, 0), "john" ]
      (Layout.split (1, 0) 5 "helloworldjohn")
  ;;
end

include Layout
