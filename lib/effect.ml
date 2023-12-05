open Core

module Effect = struct
  type t =
    | Ignore
    | Fun of (unit -> unit)
end

module Queue = struct
  open Async

  type s = Effect.t
  type t = s Deque.t

  let create () = Deque.create ~initial_length:10 ()
  let schedule eff (queue : t) = Deque.enqueue_back queue eff

  let perform_exn queue =
    let open Effect in
    match Deque.dequeue_front queue with
    | None -> raise (Invalid_argument "Nothing in queue.")
    | Some eff ->
      (match eff with
       | Ignore -> return ()
       | Fun f -> return (f ()))
  ;;

  let perform_all_exn state queue =
    Incremental.stabilize state;
    let iterator = List.init (Deque.length queue) ~f:(fun _ -> ()) in
    List.iter iterator ~f:(fun _ -> ignore (perform_exn queue >>| fun _ -> ()))
  ;;

  let ticker st queue =
    let nframes = Incremental.Var.create st 0 in
    let framerate = 60.0 in
    Async.Clock.every
      (sec (1. /. framerate))
      (fun () ->
        let temp = Incremental.Var.value nframes + 1 in
        Incremental.Var.set nframes temp;
        perform_all_exn st queue)
  ;;
end

include Effect
include Queue
