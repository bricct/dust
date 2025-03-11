open Dust
open Notty
open Styles

type state = {
  count : int;
}

let model = {
  count = 0;
}

type event = [`Tick | `Cancel | `Restart | Dust.event]

let render (w, h) s = Styles.Layout.center w h (I.string A.empty (Int.to_string s.count))

let tick_state : (state, event) Dust.State.t -> (state, event) Dust.State.t =
  Dust.State.map (fun ({ count; _ }) -> { count = count + 1; })

let cancel_timer : (state, event) Dust.State.t -> (state, event) Dust.State.t = fun s -> s
  |> State.remove_timer "tick" 
  |> State.add_timer ~iters:1 "restart" ~event:`Restart ~ms:2000

let init ds = ds 
  |> State.add_timer "tick" ~event:`Tick ~ms:1000
  |> State.add_timer "cancel" ~iters:1 ~event:`Cancel ~ms:10300

let update s e = match e with
| `Tick -> tick_state s, true
| `Cancel -> cancel_timer s, true
| `Restart -> init s, true
| _ -> s, false

let () = Dust.run ~render ~update ~model ~init ()
