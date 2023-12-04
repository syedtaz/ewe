open! Core
open! Async
open! Incremental

(** A simple version of a virtual "DOM" for the terminal. Should be a DAG and not a graph
    for forward compatibility. *)

type 'a node =
  { tag : string
  ; content : 'a -> string
  ; position : int * int
  }
[@@deriving sexp]

let diff node prev next =
  let res = node.content prev in
  let res' = node.content next in
  if String.equal res res' then None else Some res'
;;

let paint ?repaint stdout { position = y, x; content = f; tag = _ } v =
  let payload =
    if Option.value repaint ~default:false
    then Termutils.move_cursor (y, x) ^ Termutils.erasel ^ f v
    else f v
  in
  Writer.write stdout payload
;;

let fticker =
  { tag = "fticker"
  ; position = 1, 0
  ; content = (fun x -> Format.sprintf "Frame number: %d" x)
  }
;;

let sticker =
  { tag = "sticker"
  ; position = 1, 0
  ; content = (fun x -> Format.sprintf "Cutoff number: %d" (x / 50 * 50))
  }
;;
