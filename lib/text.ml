module Model = struct
  type t = { row : int; col : int} [@@deriving fields]

  let of_tuple (y, x) = { row = y; col = x}
end

module Action = struct
  type t = [`SIGWINCH]

  let apply action = match action with
    | `SIGWINCH -> Termutils.tsize ()
end

let view model =
  let open Incremental.Let_syntax in
  let row = Incremental.map model ~f:(Model.row) in
  let col = Incremental.map model ~f:(Model.col) in
  let clear = Termutils.move_cursor (1, 0) ^ Termutils.erasel in
  let%map row = row and col = col in
  (clear ^ Format.sprintf "(number of rows %d and number of cols %d)" row col)
