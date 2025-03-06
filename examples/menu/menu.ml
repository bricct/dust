open Lwt
open Dust
open Notty

type event = Dust.event

module IntMap = Map.Make(Int)
type +'a map = 'a IntMap.t

type menu_choice = {
  name : string;
  on_select : (state, event) State.t -> (state, event) State.t
}
and menu = {
  size : int;
  selected : int option;
  choices : menu_choice map
}
and screen = Menu of menu | Normal of string | Credits of string
and state = {
  content : screen
}

let empty = {
  selected = None;
  size = 0;
  choices = IntMap.empty
}

let add_choice name callback menu =
  let choices = IntMap.add menu.size { name; on_select = callback } menu.choices in
  let size = menu.size + 1 in
  { menu with choices; size }

let on_start s = s |> State.map (fun state -> { content = (Normal "Hello world") })

let on_credits s = s |> State.map (fun state -> { content = (Credits "Trey Briccetti") })

let on_quit s = s |> State.add_command `End

let model = 
  let menu = empty 
  |> add_choice "Start" on_start 
  |> add_choice "Credits" on_credits 
  |> add_choice "Quit" on_quit
  in
  {
    content = Menu menu
  }


module type Screen = sig
  type t

  val handle_keys : event -> t -> ('a, event) State.t -> ('a, event) State.t * bool

  val render : int * int -> t -> image
end

module Menu = struct
  type t = menu

  let handle_space t state = 
    match t.selected with
    | None -> state
    | Some id -> 
        let choice = IntMap.find id t.choices in
        choice.on_select state

  let handle_arrow t state dir =
    let update_menu menu = 
      let len = menu.size in
      let bound l u n = max l n |> min u in
      let move idx c = 
        let selected = Some (bound 0 (len - 1) (c + idx)) in
      { menu with selected } 
      in
      let show _ = 
        { menu with selected = Some 0 } 
      in
      match menu.selected, dir with
      | Some c, `Up -> move (-1) c
      | Some c, `Down -> move 1 c
      | None, _ -> show ()
      | _ -> menu
    in
    let new_state = { content = Menu (update_menu t) } in
    state |> State.map (fun _ -> new_state)

  let handle_escape t state =
    let new_state = let new_menu = match t.selected with
      | None -> { t with selected = Some 0 }
      | Some _ -> { t with selected = None }
    in
      { content = Menu new_menu }
    in
    state |> State.map (fun _ -> new_state)


  let handle_keys evt t state = 
    match evt with
    | `Key (`ASCII ' ', _) -> handle_space t state, true
    | `Key (`Arrow dir, _) -> handle_arrow t state dir, true
    | `Key (`Escape, _) -> handle_escape t state, true
    | _ -> state, false

  let render dim menu =
    let { selected; choices; _ } = menu in
    let fmt { name; _ } a = I.string a name |> I.pad ~t:1 ~b:1 in
    let plain = A.(fg white) in
    let highlight = A.(fg black ++ bg white) in
    let i = IntMap.fold (fun id c acc -> 
      let a = match selected with
      | Some x -> if x = id then highlight else plain
      | _ -> plain
      in
      I.( acc <-> fmt c a )) choices I.empty 
    in
    i
end

module Normal = struct
  type t = string
  
  type event = Dust.event

  let handle_keys evt t state = match evt with
  | `Key (`Escape, _) -> state |> State.map (fun _ -> model), true
  | _ -> state, false

  let render (w, _) s = I.string A.(fg white) s
end

let render dim state =
  match state.content with
  | Menu menu -> Menu.render dim menu
  | Normal s | Credits s -> Normal.render dim s

let update (state : (state, event) State.t)  (evt : event) : (state, event) State.t * bool = 
  let inner = State.extract state in
  match inner.content with
  | Menu m -> Menu.handle_keys evt m state
  | Normal s | Credits s -> Normal.handle_keys evt s state

let help_text = 
  [ 
    ("↑/↓", "Up/Down");
    ("spc", "Select");
    ("esc", "Back");
  ] |> Common.controls

let render_with_layout d s = render d s |> Common.layout d "Menu" help_text

let () = Dust.run ~model ~render:render_with_layout ~update ()
