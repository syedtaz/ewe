open Async

module Termutils = struct
  external c_tsize : unit -> int = "tsize" [@@noalloc]

  (** [tsize] uses ioctl to get the number of rows and columns of the current window. *)
  let tsize () =
    let temp = c_tsize () in
    temp lsr 16, temp land 255
  ;;

  (** [hcursor] hides the cursor. *)
  let hcursor writer = Writer.write writer "\x1b[?25l"

  let rgb r g b = Format.sprintf "\x1b[38;2;%d;%d;%dm" r g b
  let clearfmt = "\x1b[0m"

  (** [scursor] shows the cursor. *)
  let scursor writer = Writer.write writer "\x1b[?25h"

  (** [delete] moves the cursor one column back. *)
  let delete = "\x1b[1D"

  let erase_screen writer = Writer.write writer "\x1b[2J"

  (** [erasel_till_cursor] clears the current line from the starting column upto the cursor. *)
  let erasel_till_cursor = "\x1b[1K"

  (** [erasel_till_cursor_reset] clears the current line from the starting column upto the cursor and moves the cursor back. *)
  let erasel_till_cursor_reset = "\x1b[1K\r"

  (** [erasel] clears the current line. *)
  let erasel = "\x1b[2K"

  (** [move_cursor] (y, x) moves the cursor to row y and column x. *)
  let move_cursor (y, x) = Format.sprintf "\x1b[%d;%dH" y x

  (** [noop] is () -> (). *)
  let noop () = ()

  module Window = struct
    let save_window_name = "\x1b[22;0t"
    let set_window_name v = Format.sprintf "\x1b]0;%s\007" v
    let restore_window_name = "\x1b[23;0t"

    let save_and_set writer name =
      Writer.write writer (save_window_name ^ set_window_name name)
    ;;
  end
end

include Termutils
