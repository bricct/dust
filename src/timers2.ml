module type Event = sig
  type event
end


module type TimerSig = sig
  type 'a t
  val compare : 'a t -> 'a t -> int
  val min : 'a t -> 'a t -> 'a t
  val equal : 'a t -> 'a t -> bool

  val get_id : 'a t -> int
  val get_event : 'a t -> 'a
  val get_callback_time_ms : 'a t -> int

  val restart_timer : 'a t -> 'a t option
end

module Timer: TimerSig = struct

  type 'a t = {
    id : int;
    ms : int;
    event : 'a;
    iters : int option;
    callback_time_ms : int;
  }

  let compare t1 t2 = Int.compare t1.ms t2.ms
  let min t1 t2 = if compare t1 t2 <= 0 then t1 else t2
  let equal t1 t2 = t1.id = t2.id

  let get_id { id; _ } = id
  let get_event { event; _ } = event
  let get_callback_time_ms { callback_time_ms; _ } = callback_time_ms

  let restart_timer ({ ms; callback_time_ms; iters; _ } as t) = 
    match iters with
    | Some i when i > 0 -> Some { t with callback_time_ms = callback_time_ms + ms; iters = Some (i - 1)}
    | Some _ -> None
    | None -> Some { t with callback_time_ms = callback_time_ms + ms }

end

module Timers = struct
  module TimerHeap = Min_heap.Make(Timer)

  type 'a timer = 'a Timer.t

  type 'a t = {
    heap : 'a TimerHeap.t;
    promise : 'a Lwt.t option;
  }
  
  let empty = {
    heap = TimerHeap.empty;
    promise = None;
  }

  let get_promise { promise; _ } = promise

  let sleep_ms ms = Lwt_unix.sleep ((Float.of_int ms) /. 1000.)

  let make_promise t = 
    let callback_time = Timer.get_callback_time_ms t in
    let current_ms = Unix.gettimeofday () *. 1000. |> Int.of_float in
    let event = Timer.get_event t in
    let lwt = match (callback_time - current_ms) with
    | x when x <= 0 -> Lwt.return event
    | x -> Lwt.((sleep_ms x) >|= fun _ -> event)
    in
    Some lwt

  let cancel_stale p = Option.iter Lwt.cancel p
    
  let add_timer timer t = 
    let { heap; promise; _ } = t in
    let old_head = TimerHeap.peek heap in
    let heap = TimerHeap.insert timer heap in
    let head = TimerHeap.peek heap in

    let promise = match old_head, head with
    | _, None -> failwith "Impossible"
    | None, Some t -> make_promise t
    | Some ot, Some t -> 
        if Timer.get_id ot = Timer.get_id t then
          promise
        else
          let () = cancel_stale promise in
          make_promise t
      in
    { heap; promise; }

  let remove_timer timer t =
    let { heap; promise; _ } = t in
    let old_head = TimerHeap.peek heap in
    let heap = TimerHeap.remove timer heap in
    let head = TimerHeap.peek heap in
    match old_head, head with
    | None, _ -> { heap; promise; }
    | Some oh, None -> 
        let () = cancel_stale promise in
        let promise = None in
        { heap; promise; }
    | Some oh, Some nh ->
        if Timer.equal oh nh then
          { heap; promise; }
        else
          let () = cancel_stale promise in
          let promise = make_promise nh in
          { heap; promise }

  let restart t =
    let { heap; _ } = t in 
    match TimerHeap.pop_opt heap with
    | (None, _) -> empty
    | (Some timer, heap) -> match Timer.restart_timer timer with
      | Some timer -> add_timer timer { heap; promise = None; }
      | None -> 
          (let promise = match TimerHeap.peek heap with
          | Some head -> make_promise head
          | None -> None
          in
          { heap; promise })

end
