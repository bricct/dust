open Notty
open Handles
open Command
open Lwt.Infix
open Timers

module T = Notty_lwt.Term

 
type 'a task = { 
  task : (task_info * 'a) Lwt.t;
}

type 'a stream = {
  stream : (task_info * 'a) Lwt.t;
  restart : (unit -> 'a stream)
}

let make_task f id : 'a task = 
  let info = Task, id in
  {
    task = f () >|= (fun res -> (info, res))
  }

let rec make_stream f id : 'a stream = 
  let info = Stream, id in
  { 
    stream = f () >|= (fun res -> (info, res));
    restart = fun () -> make_stream f id
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


type 'b timers = {
  handles : TimerSet.t;
  heap: 'b Timers.t;
}

module State = struct

  type ('a, 'b) t = {
    internal : 'a;
    dim : int * int;
    commands : 'b CommandQueue.t;
    streams : 'b stream TaskMap.t;
    tasks : 'b task TaskMap.t;
    timers :  'b timers;
}
 constraint 'b = [> event]


  let add_task : (unit -> 'b Lwt.t) -> string -> ('a, 'b) t -> ('a, 'b) t = fun f id state ->
    let handle = TaskMap.make_handle id state.tasks in
    let task = make_task f handle in
    let tasks = TaskMap.add handle task state.tasks in
    { state with tasks }

  let add_command : 'b -> ('a, 'b) t -> ('a, 'b) t = fun command state ->
    let commands = CommandQueue.push command state.commands in
    { state with commands; }

  let add_stream : (unit -> 'b Lwt.t) -> string -> ('a, 'b) t -> ('a, 'b) t = fun f id state ->
    let handle = TaskMap.make_handle id state.tasks in
    let stream = make_stream f handle in
    let streams = TaskMap.add handle stream state.streams in
    { state with streams; }

  let add_timer :  ?iters:int -> string -> ms:int -> event:'b -> ('a, 'b) t -> ('a, 'b) t = fun ?iters id ~ms ~event state -> 
    match iters with
    | Some x when x <= 0 -> state
    | _ ->
    match ms with
    | x when x <= 0 -> state
    | _ ->
    let { handles; heap; } = state.timers in
    let handle = TimerSet.make_handle id handles in
    let heap = Timers.add ?iters handle ~ms ~event heap in
    let handles = TimerSet.add handle handles in
    let timers = { handles; heap; } in
    { state with timers; }

  let remove_task id state = 
    let handle = TaskMap.get_handle id state.tasks in
    let tasks = TaskMap.remove ~on_remove:(fun t -> Lwt.cancel t.task) handle state.tasks in
    { state with tasks; }

  let try_remove_task id state = 
    let handle_opt = TaskMap.get_handle_opt id state.tasks in
    match handle_opt with
    | None -> state
    | Some handle ->
    let tasks = TaskMap.remove ~on_remove:(fun t -> Lwt.cancel t.task) handle state.tasks in
    { state with tasks; }
      
  let remove_stream id state =
    let handle = TaskMap.get_handle id state.streams in
    let streams = TaskMap.remove ~on_remove:(fun t -> Lwt.cancel t.stream) handle state.streams in
    { state with streams; }

  let remove_timer id state =
    let { handles; heap; } = state.timers in
    let handle = TimerSet.get_handle id handles in
    let heap = Timers.remove handle heap in
    let handles = TimerSet.remove handle handles in
    let timers = { heap; handles; } in
    { state with timers; }

  let return internal = 
    {
      internal;
      dim = (0, 0);
      commands = CommandQueue.empty;
      tasks = TaskMap.empty;
      streams = TaskMap.empty;
      timers = {
        handles = TimerSet.empty;
        heap = Timers.empty;
      };
    }

  let extract state = state.internal

  let dim state = state.dim

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
    let handle_event (state : ('a, 'b) state) evt frame = 
      match evt with
      | `End | `Key (`ASCII 'Q', [`Ctrl]) -> Lwt.return_unit
      | `Resize dim as evt -> invoke ({ state with dim }) evt frame
      | evt -> invoke state evt frame
    in
    let command, commands = CommandQueue.pop_opt state.commands in
    match command with
    | Some evt -> handle_event { state with commands } evt frame
    | None ->
      let lwts = 
        let task_lwts = TaskMap.list_map (fun { task; _ } -> task) state.tasks in
        let stream_lwts = TaskMap.list_map (fun  { stream; _ } -> stream) state.streams in
        let timer_lwt = Timers.get_promise state.timers.heap in
        let lwt_list = task_lwts @ stream_lwts in
        match timer_lwt with 
        | None -> lwt_list
        | Some t_lwt -> t_lwt :: lwt_list
      in
      let finished_lwt = Lwt.choose lwts in
      finished_lwt >>= fun (info, evt) -> 
        let state = match info with
        | Stream, id ->
          let returned_stream = TaskMap.find id state.streams in
          let restarted_stream = returned_stream.restart () in
          let streams : 'b stream TaskMap.t = TaskMap.update id (Option.map (fun _ -> restarted_stream)) state.streams in
          { state with streams; }
        | Task, id -> 
            let tasks : 'b task TaskMap.t = TaskMap.remove id state.tasks in 
            { state with tasks; }
        | Timer, id ->
            let { heap; handles; } = state.timers in
            let timer_removed, heap = Timers.restart heap in
            let handles = if timer_removed then TimerSet.remove id handles else handles in
            let timers = { heap; handles; } in
            { state with timers; }
        in
        handle_event state evt frame
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
  let state = model |> State.return |> State.add_stream events "__keyboard" |> init in
  let proc = process ~term ~init:state ~render
  ~f:(fun state e -> 
    let state', dirty = update state e in
    let force_redraw = match e with `Resize _ -> true | _ -> false in
    let cmd = 
      if dirty || force_redraw then Redraw
      else Continue
    in
    cmd, state')
  in
  Lwt_main.run proc

