open Core

type t = SIGWINCH

let lift = function
  | SIGWINCH -> Signal.of_caml_int 28
;;
