open Notty
open Lwt.Infix

module T = Notty_lwt.Term

type background_work = Task | Stream | Timer


module AsyncId : sig
  include Map.OrderedType
  val first : t
  val next : t -> t
end = struct
  type t = int
  let first = 0
  let next id = id + 1
  let compare id1 id2 = Int.compare id1 id2
end

type async_id = AsyncId.t

type dust_handle = background_work * async_id
 
type 'a task = { 
  handle : dust_handle;
  task : (dust_handle * 'a) Lwt.t;
}

type 'a stream = {
  handle : dust_handle;
  stream : (dust_handle * 'a) Lwt.t;
  restart : (unit -> 'a stream)
}

type 'a timer = {
  handle : dust_handle;
  iters : int;
  timer : (dust_handle * 'a) Lwt.t;
  restart : (unit -> 'a timer);
}

module AsyncIdMap = Map.Make(AsyncId)


let make_task f id : 'a task = 
  let handle = Task, id in
  {
    handle;
    task = f () >|= (fun res -> (handle, res))
  }

let rec make_stream f id : 'a stream = 
  let handle = Stream, id in
  { 
    handle;
    stream = f () >|= (fun res -> (handle, res));
    restart = fun () -> make_stream f id
  }

let rec make_timer e id ms iters : 'a timer = 
  let handle = Timer, id in
  let sec = Float.of_int ms /. 1000. in
  {
    handle;
    iters;
    timer = Lwt_unix.sleep sec >|= (fun _ -> (handle, e));
    restart = fun () -> make_timer e id ms (iters - 1)
  }

let event e = Lwt_stream.get (T.events e) >|= function
  | Some (`Resize dim) -> `Resize dim
  | Some (#Unescape.event as x) -> x
  | None -> `End

type event = [ `End
       | `Key of Notty.Unescape.key
       | `Mouse of Notty.Unescape.mouse
       | `Paste of Notty.Unescape.paste
       | `Resize of int * int ]

module Timers = Timers2.Timers

module State = struct

  type ('a, 'b) t = {
    internal : 'a;
    dim : int * int;
    next_task_id : async_id;
    next_stream_id : async_id;
    next_timer_id : async_id;
    streams : 'b stream AsyncIdMap.t;
    tasks : 'b task AsyncIdMap.t;
    timers : 'b timer AsyncIdMap.t;
  }
 constraint 'b = [> event]


  let add_task : (unit -> 'b Lwt.t) -> ('a, 'b) t -> ('a, 'b) t = fun task state ->
    let id = state.next_task_id in
    let task = make_task task id in
    { state with next_task_id = AsyncId.next id; tasks = AsyncIdMap.add id task state.tasks }

  let add_command : 'b -> ('a, 'b) t -> ('a, 'b) t = fun e s ->
    let task = (fun () -> Lwt.return e) in
    add_task task s

  let add_stream : (unit -> 'b Lwt.t) -> ('a, 'b) t -> ('a, 'b) t = fun f state ->
    let id = state.next_stream_id in
    let stream = make_stream f id in
    { state with next_stream_id = AsyncId.next id; streams = AsyncIdMap.add id stream state.streams }

  let add_timer : 'b -> ms:int -> ?iters:int -> ('a, 'b) t -> dust_handle * ('a, 'b) t = fun e ~ms ?(iters = 0) state ->
    let id = state.next_timer_id in
    let timer = make_timer e id ms (iters - 1) in
    ((Timer, id), { state with next_timer_id = AsyncId.next id; timers = AsyncIdMap.add id timer state.timers })

  let remove : dust_handle -> ('a, 'b) t -> ('a, 'b) t = fun (bw, id) s ->
    match bw with
    | Task -> 
        AsyncIdMap.find_opt id s.tasks |> Option.fold ~none:() ~some:(fun task -> Lwt.cancel task.task);
        { s with tasks = AsyncIdMap.remove id s.tasks }
    | Stream -> 
        AsyncIdMap.find_opt id s.streams |> Option.fold ~none:() ~some:(fun stream -> Lwt.cancel stream.stream);
        { s with streams = AsyncIdMap.remove id s.streams }
    | Timer -> 
        AsyncIdMap.find_opt id s.timers |> Option.fold ~none:() ~some:(fun timer -> Lwt.cancel timer.timer);
        { s with timers = AsyncIdMap.remove id s.timers }

  let return internal = 
    {
      internal;
      dim = (0, 0);
      next_task_id = AsyncId.first;
      next_stream_id = AsyncId.first;
      next_timer_id = AsyncId.first;
      tasks = AsyncIdMap.empty;
      streams = AsyncIdMap.empty;
      timers = AsyncIdMap.empty;
    }

  let extract state = state.internal

  let extend (f : ('a, 'b) t -> 'c) state = { state with internal = f state }

  let map (f : ('a -> 'b)) = extend (fun s -> s |> extract |> f)

  let set state internal = map (fun _ -> internal) state

end

type ('a, 'b) state = ('a, 'b) State.t

let draw_screen term render dim s frame =
  let new_frame = (render dim s) in
  let action () = 
    if not @@ I.equal new_frame frame then 
      T.image term (render dim s)
    else 
      Lwt.return_unit
  in
  action () >|= fun _ -> new_frame

type command = Redraw | Continue

let process :
          term:(T.t) ->
          f:(('a, 'b) state -> 'b -> command * ('a, 'b) state) -> 
          render:(int * int -> 'a -> image) ->
          init:('a, 'b) state ->
          unit Lwt.t = fun ~term ~f ~render ~init ->
  let rec loop : ('a, 'b) state -> image -> unit Lwt.t = fun state frame ->
    let lwts = 
      let task_lwts = AsyncIdMap.fold (fun _ { task; _ } acc -> task :: acc) state.tasks [] in
      let stream_lwts = AsyncIdMap.fold (fun _ { stream; _} acc -> stream :: acc) state.streams [] in
      let timer_lwts = AsyncIdMap.fold (fun _ { timer; _} acc -> timer :: acc) state.timers [] in
      List.concat [task_lwts; stream_lwts; timer_lwts]
    in
    let finished_lwt = Lwt.choose lwts in
    finished_lwt >>= fun (handle, res) -> 
      let state = match handle with
      | Stream, id ->
        let returned_stream = AsyncIdMap.find id state.streams in
        let restarted_stream = returned_stream.restart () in
        let streams : 'b stream AsyncIdMap.t = AsyncIdMap.update id (Option.map (fun _ -> restarted_stream)) state.streams in
        { state with streams; }
      | Task, id -> 
          let tasks : 'b task AsyncIdMap.t = AsyncIdMap.remove id state.tasks in 
          { state with tasks; }
      | Timer, id ->
          let timer = AsyncIdMap.find id state.timers in
          let restarted_timer = if timer.iters = 0 then None else Some (timer.restart ()) in
          let timers : 'b timer AsyncIdMap.t = match restarted_timer with
          | None -> AsyncIdMap.remove id state.timers
          | Some timer -> AsyncIdMap.update id (Option.map (fun _ -> timer)) state.timers in
          { state with timers; }
      in
      match res with
      | `End | `Key (`ASCII 'Q', [`Ctrl]) -> Lwt.return_unit
      | `Resize dim as evt -> invoke ({ state with dim }) evt frame
      | evt -> invoke state evt frame
  and invoke s e frame =
    let cmd, s = f s e in
    match cmd with
    | Continue -> loop s frame
    | Redraw ->
        draw_screen term render s.dim s.internal frame >>=
          fun new_frame ->
            loop s new_frame
  in
  let size = T.size term in
  draw_screen term render size init.internal I.empty >>= fun _ ->
    loop { init with dim = size } I.empty

let run ~render ~model ~update ?(init = Fun.id) () =
  let term = T.create () in
  let events = (fun () -> event term) in
  let state = model |> State.return |> State.add_stream events |> init in
  let proc = process ~term ~init:state ~render
  ~f:(fun state e -> 
    let state', dirty = update state e in
    let cmd = 
      if dirty then Redraw
      else Continue
    in
    cmd, state')
  in
  Lwt_main.run proc

