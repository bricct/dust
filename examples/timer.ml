open Notty

type state = { 
  remaining_ms : int;
  running : bool;
}

let state = { 
  remaining_ms = 10000;
  running = false;
}

let init = Dust.make_state state 
        |> Dust.add_timer 0.01 `Timer

let handle_space s = 
  { s with running = not s.running }

let handle_backspace _ = 
  { running = false; remaining_ms = 10000 }

let tick_timer s =
  if s.running && s.remaining_ms > 0 then
    { running = true; remaining_ms = s.remaining_ms - 10 }
  else
    { s with running = false; }

let update _ evt = 
  let id s = s in
  match evt with
  | `Key (`ASCII ' ', _) -> handle_space, true
  | `Key (`Backspace, _) -> handle_backspace, true
  | `Timer -> tick_timer, true
  | _ -> id, true

let render _ state = 
  let fmt_time = 
    Printf.sprintf "%d" (state.remaining_ms / 1000) 
    ^ "." 
    ^ Printf.sprintf "%.3d" (state.remaining_ms mod 1000) in
  let time = I.string A.(bg white ++ fg (gray 8)) @@ fmt_time in
  I.pad ~l:8 ~t:2 time

let () = Dust.run ~init ~render ~update ()




