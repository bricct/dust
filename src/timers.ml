type 'a timer = {
  id : int;
  ms : int;
  event : 'a;
}

type 'a t = {
  timers : 'a timer list;
  timestamp : int;
  promise : 'a Lwt.t option;
}

let compare t1 t2 = Int.compare t1.ms t2.ms
let min t1 t2 = if compare t1 t2 <= 0 then t1 else t2

let get_promise { promise; _ } = promise


let add_timer timer t = 
  let timers = timer :: t.timers in
  let next_timer = List.fold_left (fun next timer -> min timer next) timer t.timers in
  let wait_s = (Float.of_int next_timer.ms) /. 1000. in
  let promise = Some Lwt.(Lwt_unix.sleep wait_s >|= Fun.const next_timer.event) in
  let timestamp = Unix.gettimeofday () *. 1000. |> Int.of_float in
  { timers; timestamp; promise; }


let remove_timer timer t =
  let timers = List.filter (fun { id; _ } -> id != timer.id) t.timers in
  let next_timer = List.fold_left (fun next timer -> min timer next) timer t.timers in
  let wait_s = (Float.of_int next_timer.ms) /. 1000. in
  let promise = Some Lwt.(Lwt_unix.sleep wait_s >|= Fun.const next_timer.event) in
  let timestamp = Unix.gettimeofday () *. 1000. |> Int.of_float in
  { timers; timestamp; promise; }
