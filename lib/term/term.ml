module Window = struct
  let save_window_name = "\x1b[22;0t"
  let set_window_name v = Format.sprintf "\x1b]0;%s\007" v
  let restore_window_name = "\x1b[23;0t"
  let save_and_set name = Runtime.Io.write_out (save_window_name ^ set_window_name name)
end

module Term = struct
  external c_tsize : unit -> int = "tsize" [@@noalloc]

  (** [tsize] uses ioctl to get the number of rows and columns of the current window. *)
  let tsize () =
    let temp = c_tsize () in
    temp lsr 16, temp land 255
  ;;

  (** [hcursor] hides the cursor. *)
  let hcursor () = Runtime.Io.write_out "\x1b[?25l"

  let rgb r g b = Format.sprintf "\x1b[38;2;%d;%d;%dm" r g b
  let clearfmt = "\x1b[0m"

  (** [scursor] shows the cursor. *)
  let scursor () = Runtime.Io.write_out "\x1b[?25h"

  (** [delete] moves the cursor one column back. *)
  let delete = "\x1b[1D"

  let erase_screen () = Runtime.Io.write_out "\x1b[2J"

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

  (** [on_startup] sets the state of the terminal at the beginning of the app.
      This includes hiding the cursor and disabling canonincal and echo mode.
      Takes an arbitrary function [f] that can be used to schedule user defined
      (possibly side-effecting) functions before the app begins properly.*)
  let startup name =
    let open Core_unix in
    let fd = File_descr.of_int 0 in
    let stdin = Terminal_io.tcgetattr fd in
    stdin.c_icanon <- false;
    stdin.c_echo <- false;
    Terminal_io.tcsetattr stdin fd ~mode:TCSANOW;
    hcursor ();
    erase_screen ();
    match name with
    | Some v -> Window.save_and_set v
    | None -> ()
  ;;

  let shutdown () =
    let open Core_unix in
    let fd = File_descr.of_int 0 in
    let stdin = Terminal_io.tcgetattr fd in
    stdin.c_icanon <- true;
    stdin.c_echo <- true;
    Terminal_io.tcsetattr stdin fd ~mode:TCSANOW;
    erase_screen ();
    scursor ();
    exit 0
    ;;
end

include Term
