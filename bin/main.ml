open Async
open Core
open Incremental

let stdin_reader = force Reader.stdin
let stdout_writer = force Writer.stdout
let echo s = Writer.write stdout_writer s

module M = Make ()

let input_char = Var.create M.State.t (Reader.Read_result.return '\x00')

let current_char = Var.watch input_char

let view = map current_char ~f:(function
    | `Ok flush when Char.equal flush '\x04' -> "\x1b[1DWe are done"
    | `Ok res -> Format.sprintf "\x1b[1D%c" res
    | `Eof -> "\x1b[1DWe are done"
  )

let count = Var.create M.State.t 0

let event_loop () =
  echo "Got the character:  \x1b[?25l";
  ignore
    (Deferred.create (fun finished ->
         let rec loop () =
           upon (Reader.read_char stdin_reader) (function
             | `Ok flush when Char.equal flush '\x04' ->
                 Ivar.fill finished ();
                 stabilize M.State.t;
                 echo
                   (Format.sprintf "\x1b[1DWe are done! Count was %d"
                      (Var.value count))
             | `Ok res ->
                 echo (Format.sprintf "\x1b[1D%c" res);
                 Var.set count (Var.value count + 1);
                 loop ()
             | `Eof ->
                 Ivar.fill finished ();
                 echo "We are done!\n")
         in
         loop ()))

let raw_mode () =
  let open Core_unix in
  let fd = File_descr.of_int 0 in
  let stdin = Terminal_io.tcgetattr fd in
  stdin.c_icanon <- false;
  stdin.c_echo <- false;
  Terminal_io.tcsetattr stdin fd ~mode:Terminal_io.TCSANOW

let () =
  raw_mode ();
  event_loop ();
  never_returns (Scheduler.go ())
