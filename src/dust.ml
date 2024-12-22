open Notty
open Lwt.Infix

module T = Notty_lwt.Term

type 'a task = Task of ('a * (unit -> 'a task)) Lwt.t

let rec task f () : 'b task = Task (f () >|= (fun r -> (r, (fun () -> task f ()))))

type ('a, 'b) state = {
  internal : 'a;
  dim : int * int;
  tasks : 'b task list
}

let map : ('a, 'b) state -> ('a -> 'a) -> ('a, 'b) state = fun state operation ->
  { state with internal = operation state.internal }

let add_task : (unit -> 'b Lwt.t) -> ('a, 'b) state -> ('a, 'b) state = fun func state ->
  { state with tasks = (task func ()) :: state.tasks }

let make_state i : ('a, 'b) state = { internal = i; dim = (0, 0); tasks = [] }

let add_timer : float -> 'b -> ('a, 'b) state -> ('a, 'b) state = fun t c state ->
  let timer = fun () -> Lwt_unix.sleep t >|= fun () -> c in
  add_task timer state

let event e = Lwt_stream.get (T.events e) >|= function
  | Some (`Resize dim) -> `Resize dim
  | Some (#Unescape.event as x) -> x
  | None -> `End

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

let process : term:(T.t) ->
          f:(('a, 'b) state -> 'b -> command * ('a, 'b) state) -> 
          render:(int * int -> 'a -> image) ->
          init:('a, 'b) state ->
          unit Lwt.t = fun ~term ~f ~render ~init ->
  let rec loop : ('a, 'b) state -> image -> unit Lwt.t = fun state frame ->
    let lwts = List.map (fun t -> match t with Task lwt -> lwt) state.tasks in
    Lwt.choose lwts >>= fun (res, callback) -> 
      let tasks : 'b task list = List.filter (fun t -> 
        match t with Task lwt ->
          match Lwt.state lwt with 
          | Sleep -> true
          | _ -> false
        ) state.tasks 
      in
      let state = { state with tasks = (callback () :: tasks) } in
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
  let event_task = task (fun () -> event term) () in
  let init = { init with tasks = event_task :: init.tasks } in
  let proc = process ~term ~init ~render
  ~f:(fun state e -> 
    let transform, dirty = update state e in
    let s' = map state transform in
    let cmd = 
      if dirty then Redraw
      else Continue
    in
    cmd, s')
  in
  Lwt_main.run proc


