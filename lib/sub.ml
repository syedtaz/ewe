module Sub = struct
  open Core
  open Async

  type 'a t =
    | Cmd of 'a
    | Quit
    | Nil

  module S = Set.Make (Char)

  let rec keypress ~f writer =
    let%bind result = Reader.read_char Runtime.Io.stdin in
    match result with
    | `Eof -> keypress ~f writer
    | `Ok c ->
      (match f (Events.Key.lift c) with
       | Cmd v ->
         ignore (Pipe.write writer v);
         keypress ~f writer
       | Nil -> keypress ~f writer
       | Quit -> Term.shutdown ()
       )
  ;;

  let rec send_msg ~f reader =
    Pipe.read' reader
    >>= fun x ->
    match x with
    | `Eof -> send_msg ~f reader
    | `Ok c ->
      Queue.iter ~f:(fun x -> f x) c;
      send_msg ~f reader
  ;;

  let start action filter ~reader ~writer =
    don't_wait_for (keypress ~f:filter writer);
    don't_wait_for (send_msg ~f:action reader)
  ;;
end

include Sub
