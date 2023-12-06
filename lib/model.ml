module Text = struct
  type t = int * int

  let of_tuple x : t = x
  let row ((r, _) : t) = r
  let col ((_, c) : t) = c
end

let view model =
  let open Incremental.Let_syntax in
  let clear = Termutils.move_cursor (1, 0) ^ Termutils.erasel in
  let%map model = model in
  (clear ^ Format.sprintf "(number of rows %d and number of cols %d)" (Text.row model) (Text.col model))
