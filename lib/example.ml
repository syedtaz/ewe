(* open Async
   open Core
   open Incremental

   let stdin_reader = force Reader.stdin
   let stdout_writer = force Writer.stdout
   let echo s = Writer.write stdout_writer s

   module M = Make ()

   let input_char = Var.create M.State.t '\x00'
   let current_char = Var.watch input_char

   let view =
     map current_char ~f:(fun x ->
         match x with
         | flush when Char.equal flush '\x04' -> "\x1b[1DWe are done"
         | res -> Format.sprintf "\x1b[1D%c" res)

   let view_o = observe view

   let event_loop () =
     let update_view v =
       Var.set input_char v;
       stabilize M.State.t;
       echo (Observer.value_exn view_o)
     in
     echo "Got the character:  \x1b[?25l";
     ignore
       (Deferred.create (fun finished ->
            let rec loop () =
              upon (Reader.read_char stdin_reader) (fun inp ->
                  match inp with
                  | `Ok value ->
                      if Char.equal value '\x04' then (
                        Ivar.fill finished ();
                        update_view value)
                      else (
                        update_view value;
                        loop ())
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
     never_returns (Scheduler.go ()) *)
