open Core

module Effect = struct
  type 'a t =
    | Ignore
    | Fun of ('a -> unit)
end

module EffectQueue = struct
  open Incremental
  open Async

  type 'a s = 'a Effect.t

  type ('a, 'b) t =
    { queue : ('a s * 'a) Deque.t
    ; var : (bool, 'b) Var.t
    }

  let create state : ('a, 'b) t =
    let v = Var.create state false in
    let _v_w = Var.watch v in
    { queue = Deque.create ~initial_length:10 (); var = v }
  ;;

  let schedule eff state { queue; var } =
    Deque.enqueue_back queue eff;
    Var.set var true;
    stabilize state
  ;;

  let perform_exn queue =
    let open Effect in
    match Deque.dequeue_front queue with
    | None -> raise (Invalid_argument "Nothing in queue.")
    | Some (eff, v) ->
      (match eff with
       | Ignore -> return ()
       | Fun f -> return (f v))
  ;;

  let perform_all_exn state { queue; var } =
    Incremental.stabilize state;
    let iterator = List.init (Deque.length queue) ~f:(fun _ -> ()) in
    List.iter iterator ~f:(fun _ -> ignore (perform_exn queue >>| fun _ -> ()));
    Var.set var false
  ;;
end

include Effect
