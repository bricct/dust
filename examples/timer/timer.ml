open Styles
open Notty
open Core
open Dust
open Lwt

type timer_state = Stopped | Started of Time_ns.t

type state = { 
  remaining_ms : int;
  timer : timer_state;
}

let model = { 
  remaining_ms = 10000;
  timer = Stopped;
}

type event = [`Timer | Dust.event]

let timer = "timer"

let init : ((state, event) State.t -> (state, event) State.t) = fun dust_state ->
  dust_state |> State.add_timer timer ~ms:20 ~event:`Timer

let handle_space s = 
  match s.timer with
  | Stopped -> 
    let ts = Time_ns.now () in
    { s with timer = Started ts }
  | Started _ ->
    { s with timer = Stopped }

let handle_backspace _ = 
  { timer = Stopped; remaining_ms = 10000 }

let tick_timer = function
  | { timer = Stopped; _ } as s -> s
  | { timer = Started last_ts; remaining_ms } ->
    let now = Time_ns.now () in
    let elapsed_span = Time_ns.diff now last_ts in
    let elapsed_ms = Int.of_float @@ Time_ns.Span.to_ms elapsed_span in
    if (elapsed_ms > remaining_ms) then
      { timer = Stopped; remaining_ms = 0 }
    else
      let remaining_ms = remaining_ms - elapsed_ms in
      { timer = Started now; remaining_ms = remaining_ms }

let help_text = 
  [ 
    ("spc", "Start/Stop");
    ("bkspc", "Reset");
  ] |> Common.controls

let render _ state = 
  let fmt_time = 
    Printf.sprintf "%d" (state.remaining_ms / 1000) 
    ^ "." 
    ^ Printf.sprintf "%.3d" (state.remaining_ms mod 1000) in
  I.string A.(bg white ++ fg (gray 8)) @@ fmt_time

let update state evt = 
  let f = match evt with
  | `Key (`ASCII ' ', _) -> handle_space
  | `Key (`Backspace, _) -> handle_backspace
  | `Timer -> tick_timer
  | _ -> Fun.id
  in
  state |> State.map f, true

let render_with_layout d s = render d s |> Common.layout d "Timer" help_text

let () = Dust.run ~model ~init ~render:render_with_layout ~update ()

