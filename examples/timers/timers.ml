open Dust
open Notty
open Styles

type state = {
  count : int;
  timer : dust_handle option;
}

let model = {
  count = 0;
  timer = None;
}

type event = [`Tick | `Cancel | `Restart | Dust.event]

let render (w, h) s = Styles.Layout.center w h (I.string A.empty (Int.to_string s.count))

let tick_state : (state, event) Dust.State.t -> (state, event) Dust.State.t =
  Dust.State.map (fun ({ count; _ } as s) -> { s with count = count + 1; })

let cancel_timer : (state, event) Dust.State.t -> (state, event) Dust.State.t = fun s ->
  let handle = (State.extract s).timer in
  let _, s = State.add_timer `Restart ~ms:2000 ~iters:1 s in
  Option.fold ~none:s ~some:(fun dh -> State.remove dh s) handle

let init ds =  
  let handle, ds = Dust.State.add_timer `Tick ~ms:1000 ds in
  let ds = State.map (fun s -> { s with timer = Some handle }) ds in
  ds |> Dust.State.add_task (fun _ -> Lwt.(Lwt_unix.sleep 10.3 >|= fun _ -> `Cancel))

let update s e = match e with
| `Tick -> tick_state s, true
| `Cancel -> cancel_timer s, true
| `Restart -> init s, true
| _ -> s, false


let () = Dust.run ~render ~update ~model ~init ()
