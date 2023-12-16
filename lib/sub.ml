module Sub = struct
  open Core

  type 'a t = Cmd of 'a | Nil

  module S = Set.Make (Char)

  let on_keypress (action : Events.Signals.World.key -> 'a t) (filter : char list) writer =
    let set = S.of_list filter in
    let filterfunc c = Set.mem set c in
    Events.Signals.Dispatch.start action filterfunc writer
  ;;

end

include Sub