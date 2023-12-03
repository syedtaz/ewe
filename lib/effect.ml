open! Core
open! Async

module Effect = struct
  type 'a t = Ignore | Callback of ('a -> unit)

  let create () : ('a t * 'a) Deque.t = Deque.create ~initial_length:10 ()

  let schedule eff queue = Deque.enqueue_back queue eff

  let perform_exn (queue : ('int t * int) Deque.t) = match Deque.dequeue_front queue with
    | None -> raise (Invalid_argument "Nothing in queue.")
    | Some(eff, v) -> match eff with
      | Ignore -> return ()
      | Callback (f : 'a -> unit) -> return (f v)

  let perform_all_exn queue =
    let iterator = List.init (Deque.length queue) ~f:(fun _ -> ()) in
    List.iter iterator ~f:(fun _ -> ignore (perform_exn queue >>| fun _ -> ()))
end

include Effect