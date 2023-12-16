module World = struct
  module Keypress = struct
    type key =
      | SPACE
      | NULL
      | A
      | B
      | C
      | D
      | E
      | F
      | G
      | H
      | I
      | J
      | K
      | L
      | M
      | N
      | O
      | P
      | Q
      | R
      | S
      | T
      | U
      | V
      | W
      | X
      | Y
      | Z
    [@@deriving sexp]

    let lift = function
      | 'a' -> A
      | 'b' -> B
      | 'c' -> C
      | 'd' -> D
      | 'e' -> E
      | 'f' -> F
      | 'g' -> G
      | 'h' -> H
      | 'i' -> I
      | 'j' -> J
      | 'k' -> K
      | 'l' -> L
      | 'm' -> M
      | 'n' -> N
      | 'o' -> O
      | 'p' -> P
      | 'q' -> Q
      | 'r' -> R
      | 's' -> S
      | 't' -> T
      | 'u' -> U
      | 'v' -> V
      | 'w' -> W
      | 'x' -> X
      | 'y' -> Y
      | 'z' -> Z
      | ' ' -> SPACE
      | '\x00' -> NULL
      | _ as x -> raise (Invalid_argument (Format.sprintf "Unrecognized key %c" x))
    ;;
  end

  include Keypress
end

module Dispatch = struct
  open Async
  open Core

  let reader, writer = Pipe.create ()
  let stdin = force Reader.stdin

  let rec keypress filter =
    let%bind result = Reader.read_char stdin in
    match result with
    | `Eof -> keypress filter
    | `Ok c ->
      let () =
        if filter c
        then Pipe.write_without_pushback writer (World.Keypress.lift c)
        else ()
      in
      keypress filter
  ;;

  let rec run f subwriter =
    Pipe.read' reader
    >>= fun x ->
    match x with
    | `Eof -> run f subwriter
    | `Ok c ->
      Queue.iter ~f:(fun x -> Pipe.write_without_pushback subwriter (f x)) c;
      run f subwriter
  ;;

  let start action filter subwriter =
    don't_wait_for (keypress filter);
    don't_wait_for (run action subwriter)
  ;;
end

include World
include Dispatch
