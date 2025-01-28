open Notty
open Dust
open Lwt

type state = {
  value : string;
  loaded : bool;
  counter : int;
}

let state = {
  value = "";
  loaded = false;
  counter = 0;
}

type event = [`Load of string | `Animate | Dust.event]

let animate = (fun () -> Lwt_unix.sleep 0.2 >|= fun _ -> `Animate)
let load_func = (fun () -> Lwt_unix.sleep 3. >|= fun _ -> `Load "Hello, World!")
let die = (fun () -> Lwt_unix.sleep 3. >|= fun _ -> `End)

let init = 
  state 
  |> State.return 
  |> State.add_task load_func
  |> State.add_task animate


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
    let img = 
      if loaded then
        I.string A.(fg white) "Loaded!"
      else
        I.(string A.(fg white) "Loading " <|> uchar A.(fg white) (spinner counter) 1 1)
    in
    img |> I.pad ~b:1
  in
  
  let slash = 
    let img = 
      if loaded then
        I.string A.(fg white) "Loaded!"
      else
        I.(string A.(fg white) "Loading " <|> char A.(fg white) (slash counter) 1 1)
    in
    img |> I.pad ~b:1
  in

  let elipses =
    let img = 
      if loaded then
        I.string A.(fg white) "Loaded!"
      else
        I.(string A.(fg white) "Loading " <|> char A.(fg white) '.' counter 1)
    in
    img |> I.pad ~b:1
  in

  I.vcat [shaded; wave; spinner; slash; elipses; ] |> I.pad ~l:5 ~t:1

let update (state : (state, event) State.t) (evt : event) = 
  let f, task =
    match evt with
    | `Load value -> (fun s -> { loaded = true; value; counter = 0 }), Some die
    | `Animate -> (fun s -> { s with counter = (s.counter + 1) mod 4 }), Some animate
    | _ -> Fun.id, None
  in
  let state = state |> State.map f in
  Option.fold ~none:state ~some:(fun t -> state |> State.add_task t) task, true
      

let () = Dust.run ~init ~render ~update ()
