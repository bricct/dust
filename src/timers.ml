open Handles

module type TimerSig = sig
  type 'a t = {
    id : Handle.t;
    ms : int;
    event : 'a;
    iters : int option;
    callback_time_ms : int;
  }
  val compare : 'a t -> 'a t -> int
  val min : 'a t -> 'a t -> 'a t
  val equal : 'a t -> 'a t -> bool

  val get_id : 'a t -> Handle.t
  val get_event : 'a t -> 'a
  val get_callback_time_ms : 'a t -> int

  val restart_timer : 'a t -> 'a t option
end

module Timer: TimerSig = struct

  type 'a t = {
    id : Handle.t;
    ms : int;
    event : 'a;
    iters : int option;
    callback_time_ms : int;
  }

  let compare t1 t2 = Int.compare t1.callback_time_ms t2.callback_time_ms
  let min t1 t2 = if compare t1 t2 <= 0 then t1 else t2
  let equal t1 t2 = Handle.equal t1.id t2.id

  let get_id { id; _ } = id
  let get_event { event; _ } = event
  let get_callback_time_ms { callback_time_ms; _ } = callback_time_ms

  let restart_timer ({ ms; callback_time_ms; iters; _ } as t) = 
    match iters with
    | Some i when i > 1 -> Some { t with callback_time_ms = callback_time_ms + ms; iters = Some (i - 1)}
    | Some _ -> None
    | None -> Some { t with callback_time_ms = callback_time_ms + ms }

end

module type TimersSig = sig
  type 'a t

  val empty : 'a t

  val get_promise : 'a t -> (task_info * 'a) Lwt.t option

  val add : ?iters:int -> Handle.t -> ms:int -> event:'a -> 'a t -> 'a t

  val remove : Handle.t -> 'a t -> 'a t
  
  val restart : 'a t -> bool * 'a t
end

module Timers : TimersSig = struct
  module TimerHeap = Min_heap.Make(Timer)

  type 'a t = {
    heap : 'a TimerHeap.t;
    promise : (task_info * 'a) Lwt.t option;
  }
  
  let empty = {
    heap = TimerHeap.empty;
    promise = None;
  }

  let get_promise { promise; _ } = promise

  let sleep_ms ms = Lwt_unix.sleep ((Float.of_int ms) /. 1000.)

  let current_ms () = Unix.gettimeofday () *. 1000. |> Int.of_float

  let make_promise t = 
    let info = (Timer, Timer.get_id t) in
    let callback_time = Timer.get_callback_time_ms t in
    let event = Timer.get_event t in
    let lwt = match (callback_time - (current_ms ())) with
    | x when x <= 0 -> Lwt.return (info, event)
    | x -> Lwt.((sleep_ms x) >|= fun _ -> (info, event))
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
        if Timer.equal ot t then
          promise
        else
          let () = cancel_stale promise in
          make_promise t
      in
    { heap; promise; }

  let remove id t =
    let { heap; promise; _ } = t in
    let old_head = TimerHeap.peek heap in
    let _, heap = TimerHeap.remove (fun t -> Handle.equal t.id id) heap in
    let head = TimerHeap.peek heap in
    match old_head, head with
    | None, _ -> { heap; promise; }
    | Some _, None -> 
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

  let add = fun ?iters id ~ms ~event -> 
    let curr_ms = current_ms () in
    let callback_time_ms = curr_ms + ms in
    let (timer : 'a Timer.t) = {
      id;
      ms;
      iters;
      event;
      callback_time_ms;
    }
    in
    add_timer timer

  let restart t =
    let { heap; _ } = t in 
    match TimerHeap.pop_opt heap with
    | (None, _) -> true, empty
    | (Some timer, heap) -> 
      let promise = match TimerHeap.peek heap with
      | Some head -> make_promise head
      | None -> None
      in
      match Timer.restart_timer timer with
      | Some timer -> false, add_timer timer { heap; promise; }
      | None -> true, { heap; promise }
end
