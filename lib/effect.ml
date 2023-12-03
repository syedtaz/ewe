open! Core
open! Async

module Effect = struct
  type 'a t = ..
  type 'a t += Ignore : unit t | Fun : ('a -> unit) -> 'a t

  let queue : ('a t * 'a) Deque.t = Deque.create ~initial_length:10 ()

  let schedule eff queue = Deque.enqueue_back queue eff

  let perform_exn (queue : ('a t * 'a) Deque.t) = match Deque.dequeue_front queue with
    | None -> raise (Invalid_argument "Nothing in queue.")
    | Some(eff, v) -> match eff with
      | Ignore -> ()
      | Fun f -> f v
      | _ -> raise (Invalid_argument "Unknown variant.")
end

include Effect