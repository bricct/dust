open Notty
open Lwt.Infix

module T = Notty_lwt.Term

type background_work = Task | Stream
type handle = background_work * int

type 'a task = { 
  handle : handle;
  task : (handle * 'a) Lwt.t;
}

type 'a stream = {
  handle : handle;
  stream : (handle * 'a) Lwt.t;
  restart : (unit -> 'a stream)
}

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

let event e = Lwt_stream.get (T.events e) >|= function
  | Some (`Resize dim) -> `Resize dim
  | Some (#Unescape.event as x) -> x
  | None -> `End

type event = [ `End
       | `Key of Notty.Unescape.key
       | `Mouse of Notty.Unescape.mouse
       | `Paste of Notty.Unescape.paste
       | `Resize of int * int ]

module State = struct

  type ('a, 'b) t = {
    internal : 'a;
    dim : int * int;
    next_task_id : int;
    next_stream_id : int;
    streams : 'b stream list;
    tasks : 'b task list
  }
 constraint 'b = [> event]

  let map : ('a -> 'b) -> ('a, 'event) t -> ('b, 'event) t = 
    fun f state -> { state with internal = f state.internal }

  let add_task : (unit -> 'b Lwt.t) -> ('a, 'b) t -> ('a, 'b) t = fun task state ->
    let id = state.next_task_id in
    let task = make_task task id in
    { state with next_task_id = id + 1; tasks = task :: state.tasks }

  let add_command : 'b -> ('a, 'b) t -> ('a, 'b) t = fun e s ->
    let task = (fun () -> Lwt.return e) in
    add_task task s

  let add_stream : (unit -> 'b Lwt.t) -> ('a, 'b) t -> ('a, 'b) t = fun f state ->
    let id = state.next_stream_id in
    let stream = make_stream f id in
    { state with next_stream_id = id + 1; streams = stream :: state.streams }

  let return internal = 
    {
      internal;
      dim = (0, 0);
      next_task_id = 0;
      next_stream_id = 0;
      tasks = [];
      streams = [];
    }

  let get state = state.internal

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
      let task_lwts = List.map (fun { task; _ } -> task) state.tasks in
      let stream_lwts = List.map (fun { stream; _} -> stream) state.streams in
      List.concat [task_lwts; stream_lwts]
    in
    let finished_lwt = Lwt.choose lwts in
    finished_lwt >>= fun (handle, res) -> 
      let state = match handle with
      | Stream, id ->
        let returned_stream = List.find (fun ({ handle = (_, stream_id); _ } : 'b stream) -> id = stream_id) state.streams in
        let restarted_stream = returned_stream.restart () in
        let streams : 'b stream list = 
          restarted_stream :: (List.filter (fun ({ handle = (_, stream_id); _ } : 'b stream) -> id != stream_id) state.streams) in
        { state with streams; }
      | Task, id -> 
          let tasks : 'b task list = 
            List.filter (fun ({ handle = (_, task_id); _ } : 'b task) -> id != task_id) state.tasks in
        { state with tasks; }
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

let run ~render ~init ~update () =
  let term = T.create () in
  let events = (fun () -> event term) in
  let init = init |> State.add_stream events in
  let proc = process ~term ~init ~render
  ~f:(fun state e -> 
    let state', dirty = update state e in
    let cmd = 
      if dirty then Redraw
      else Continue
    in
    cmd, state')
  in
  Lwt_main.run proc

