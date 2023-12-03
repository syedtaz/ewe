open Async

module Termutils = struct
  (** [hcursor] hides the cursor. *)
  let hcursor writer = Writer.write writer "\x1b[?25l"

  (** [delete] moves the cursor one column back. *)
  let delete = "\x1b[1D"

  (** [erasel_till_cursor] clears the current line from the starting column upto the cursor. *)
  let erasel_till_cursor = "\x1b[1K"

  (** [erasel_till_cursor_reset] clears the current line from the starting column upto the cursor and moves the cursor back. *)
  let erasel_till_cursor_reset = "\x1b[1K\r"

  (** [erasel] clears the current line. *)
  let erasel = "\x1b[2K"

  let move_cursor (y, x) = Format.sprintf "\x1b[%d;%dH" y x

  (** [noop] is () -> (). *)
  let noop () = ()
end

include Termutils
