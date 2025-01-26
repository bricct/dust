open Notty
open Lwt
open Core

type state = { 
  remaining_ms : int;
  running : bool;
}

let state = { 
  remaining_ms = 10000;
  running = false;
}

type event = [`Timer | Dust.event]

let timer = (fun () -> 
  Lwt_unix.sleep 0.01 >|= fun _ ->
    Core.Tim
    `Timer)

let init : (state, event) Dust.State.t =
  Dust.State.return state |> Dust.State.add_stream timer

let handle_space s = 
  { s with running = not s.running }

let handle_backspace _ = 
  { running = false; remaining_ms = 10000 }

let tick_timer s =
  if s.running && s.remaining_ms > 0 then
    { running = true; remaining_ms = s.remaining_ms - 10 }
  else
    { s with running = false; }

let update state evt = 
  let f = match evt with
  | `Key (`ASCII ' ', _) -> handle_space
  | `Key (`Backspace, _) -> handle_backspace
  | `Timer -> tick_timer
  | _ -> Fun.id
  in
  state |> Dust.State.map f, true

let render _ state = 
  let fmt_time = 
    Printf.sprintf "%d" (state.remaining_ms / 1000) 
    ^ "." 
    ^ Printf.sprintf "%.3d" (state.remaining_ms mod 1000) in
  let time = I.string A.(bg white ++ fg (gray 8)) @@ fmt_time in
  I.pad ~l:8 ~t:2 time

let () = Dust.run ~init ~render ~update ()




