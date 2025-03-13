open Common
open Styles
open Notty
open Dust
open Lwt

type state = {
  value : string;
  loaded : bool;
  counter : int;
}

let model = {
  value = "";
  loaded = false;
  counter = 0;
}

type event = [`Load of string | `Animate | Dust.event]

let animate = "animate"
let animate_ms = 200

let load = "load"
let load_ms = 3000

let die = "die"
let die_ms = 3000

let init state = state
  |> State.add_timer load ~iters:1 ~ms:load_ms ~event:(`Load "Hello World!")
  |> State.add_timer animate ~ms:animate_ms ~event:`Animate


let shade c = 
  let uchar = match c mod 4 with
  | 0 -> 0x2591
  | 1 -> 0x2592
  | 2 -> 0x2593 
  | _ -> 0x2588 
  in
  Uchar.of_int uchar

let wave c =
  let uchar = match c mod 4 with
  | 0 -> 0x2581
  | 1 -> 0x2582
  | 2 -> 0x2584 
  | _ -> 0x2583
  in
  Uchar.of_int uchar


let spinner c = 
  let uchar = match c mod 4 with
  | 0 -> 0x2596
  | 1 -> 0x2598
  | 2 -> 0x259D
  | _ -> 0x2597
  in
  Uchar.of_int uchar

let slash c = 
  match c mod 4 with
  | 0 -> '|'
  | 1 -> '/'
  | 2 -> '-'
  | _ -> '\\'

let render _ state = 
  let { value; loaded; counter; _ } = state in

  let load_str l v x = 
    let i = 
      if l then
        I.string A.(fg white) v
      else
        I.uchar A.(fg white) (x counter) 10 1
    in
    I.pad ~b:1 i
  in

  let shaded = load_str loaded value shade in
  let wave = load_str loaded value wave in

  let spinner = 
    if loaded then
      I.string A.(fg white) "Loaded!"
    else
      I.(string A.(fg white) "Loading " <|> uchar A.(fg white) (spinner counter) 1 1)
  in
  
  let slash = 
    if loaded then
      I.string A.(fg white) "Loaded!"
    else
      I.(string A.(fg white) "Loading " <|> char A.(fg white) (slash counter) 1 1)
  in

  let elipses =
    if loaded then
      I.string A.(fg white) "Loaded!"
    else
      I.(string A.(fg white) "Loading " <|> char A.(fg white) '.' counter 1)
  in

  let void = I.void 12 1 in

  Layout.flex_v ~gap:1 [shaded; wave; spinner; slash; elipses; void]

let update (state : (state, event) State.t) (evt : event) = 
  let f, should_die =
    match evt with
    | `Load value -> (fun s -> { loaded = true; value; counter = 0 }), true
    | `Animate -> (fun s -> { s with counter = (s.counter + 1) mod 4 }), false
    | _ -> Fun.id, false
  in
  let state = state |> State.map f in
  let state = 
    if should_die then
      state |> State.add_timer die ~ms:die_ms ~event:`End
    else
      state
  in
  state, true
      
let render_with_layout d s = render d s |> layout d "Load" I.empty

let () = Dust.run ~model ~init ~render:render_with_layout ~update ()
