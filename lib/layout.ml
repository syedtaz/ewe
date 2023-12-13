module Layout = struct
  open Core

  type t = (int * int) * (int * int) * string [@@deriving compare, sexp]

  let stdout =
    let open Async in
    force Writer.stdout
  ;;

  let split (crow, ccol) size str : t list =
    let rec aux (crow, ccol) acc str =
      let len = String.length str in
      if len > size
      then
        if String.contains str '\n'
        then newline (crow, ccol) acc str
        else (
          let chunk = String.sub ~pos:0 ~len:size str ^ "\n" in
          let rest = String.sub ~pos:size ~len:(len - size) str in
          aux (crow + 1, ccol) (((crow, ccol), (crow, ccol + size), chunk) :: acc) rest)
      else ((crow, ccol), (crow, ccol + len), str) :: acc
    and newline (crow, ccol) acc str =
      let chunk, rest = String.lsplit2_exn str ~on:'\n' in
      let nchunk = chunk ^ "\n" in
      let ncol = String.length nchunk in
      aux (crow + 1, ccol) (((crow, ccol), (crow, ccol + ncol), nchunk) :: acc) rest
    in
    List.rev (aux (crow, ccol) [] str)
  ;;

  let paint (els : t list) =
    String.concat
    @@ List.map els ~f:(fun (cursor, _, payload) ->
      Termutils.move_cursor cursor ^ payload)
  ;;

  (* let generate (n : Vdom.t) : t list =
    let f acc (x : string option) =
      let y = Option.value x ~default:"" in
      split (1, 0) 5 y @ acc
    in
    Vdom.fold_value n [] ~f *)
end

module Tests = struct
  open Core

  let%test_unit "split_no_split" =
    [%test_eq: Layout.t list] [ (1, 0), (1, 5), "hello" ] (Layout.split (1, 0) 10 "hello")
  ;;

  let%test_unit "split_one_split" =
    [%test_eq: Layout.t list]
      [ (1, 0), (1, 5), "hello\n"; (2, 0), (2, 5), "world" ]
      (Layout.split (1, 0) 5 "helloworld")
  ;;

  let%test_unit "split_multi_split" =
    [%test_eq: Layout.t list]
      [ (1, 0), (1, 5), "hello\n"; (2, 0), (2, 5), "world\n"; (3, 0), (3, 4), "john" ]
      (Layout.split (1, 0) 5 "helloworldjohn")
  ;;

  let%test_unit "split_multi_split_2" =
    [%test_eq: Layout.t list]
      [ (1, 0), (1, 6), "hello\n"; (2, 0), (2, 5), "world" ]
      (Layout.split (1, 0) 10 "hello\nworld")
  ;;

  (* let simple_vdom =
     let open Vdom in
     text ~id:"id" ~grid:(colidx 0) ~attrs:[] "hello" []
     ;;

     let multi_vdom =
     let open Vdom in
     text
     ~id:"id"
     ~grid:(colidx 0)
     ~attrs:[]
     "hello"
     [ text ~id:"id2" ~attrs:[] ~grid:(colidx 0) "world" [] ]
     ;; *)

  (* let%test_unit "generate_one" =
     [%test_eq: Layout.t list] [ (1, 0), (1, 5), "hello" ] (Layout.generate simple_vdom)
     ;;

     let%test_unit "generate_multi" =
     [%test_eq: Layout.t list]
     (* TODO! FIX THIS *)
     [ (1, 0), (1, 5), "hello"; (1, 0), (1, 5), "world" ]
     (Layout.generate multi_vdom) *)
end

include Layout
