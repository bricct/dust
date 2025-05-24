open Notty
open Dust


type screen_choice = Menu | Play | Settings | Quit

type event = [`Animate | `Route of screen_choice | Dust.event]

let string_of_choice = function
| Menu -> "Menu"
| Play -> "Play"
| Settings -> "Settings"
| Quit -> "Quit"

module type Screen = sig
  type state

  val empty : int *  int -> state
  val update : state -> event -> state * event option
  val render : state -> int * int -> image
end

type screen = 
  | Screen : 
    {
      state : 's;
      ops : (module Screen with type state = 's);
    } -> screen

module AbstractMenu = struct
  type state = 
    { 
      prev : screen_choice list;
      next : screen_choice list;
      curr : screen_choice;
      selected : bool;
    }

  let empty options dim = 
    match options with
    | [] -> assert false
    | h :: t ->
    { next = t; prev = []; curr = h; selected = false; }

  let move_prev ({ prev; next; curr; selected; _ } as state) =
  match selected, prev, curr with
  | false, _, _ -> { state with selected = true }
  | _, [], _ -> state
  | _, curr :: prev, n -> { state with curr; prev; next = n :: next; }

  let move_next ({ prev; next; curr; selected; _ } as state) =
  match selected, next, curr with
  | false, _, _ -> { state with selected = true }
  | _, [], _ -> state
  | _, curr :: next, p -> { state with curr; prev = p :: prev; next; }

  let change_selection state = function
  | `Up -> move_prev state
  | `Down -> move_next state
  | _ -> state


  let update state: event -> state * event option = function
  | `Key (`Arrow dir, _) -> change_selection state dir, None
  | `Key (`Escape, _) -> { state with selected = false; }, None
  | `Key (`ASCII ' ', _) when state.selected = true -> state, Some (`Route state.curr)
  | `Key (`ASCII ' ', _) when state.selected = false -> { state with selected = true }, None
  | _ -> state, None

  let render { prev; next; curr; selected; _ } _ = 
    let highlight = A.(bg white ++ fg black) in
    let regular = A.empty in
    let fmt_choice attr s = string_of_choice s |> I.string attr in
    let map_choices = List.map (fmt_choice regular) in
    let curr = 
      let a = if selected then highlight else regular in 
      fmt_choice a curr 
    in
    let prev = List.rev @@ map_choices prev in
    let next = map_choices next in 
    Styles.Layout.flex_v ~gap:1 ~align:`Left (prev @ [curr] @ next)
end

module MainMenu : Screen = struct
  include AbstractMenu
  let main_menu_options = [Play; Settings; Quit]
  let empty = empty main_menu_options
end


module Play : Screen = struct
  type state = 
    {
      position: int * int;
      dim : int * int;
    }

  let empty dim = { position = (0, 0); dim; }
  
  let bound l u v = min u @@ max v l

  let resize ({ position = (x, y); _ }) ((w, h) as dim) = 
  let x = bound 0 w x in
  let y = bound 0 h y in
  { position = (x, y); dim }

  let try_move ({ position = (x, y); dim = (w, h)} as state) dir = 
    let x, y = match dir with
    | `Left -> bound 0 w (x - 1), y
    | `Right -> bound 0 w (x + 1), y
    | `Up -> x, bound 0 h (y - 1)
    | `Down -> x, bound 0 h (y + 1)
    in
    { state with position = (x, y) }


  let update state: event -> state * event option = function
  | `Resize dim -> resize state dim, None
  | `Key (`Arrow dir, _) -> try_move state dir, None
  | `Key (`Escape, _) -> state, Some (`Route Menu)
  | _ -> state, None

  let render { position = (x, y); _ } (w, h) = 
    let position = Printf.sprintf "(%d, %d), dim: (%d, %d)" x y w h in
    I.string A.empty position
end
  


type state = {
  current_screen : screen;
}

let create_play dim = 
  Screen ({ state = Play.empty dim; ops = (module Play) })

let create_settings dim = 
  Screen ({ state = Play.empty dim; ops = (module Play) })

let create_main_menu dim = 
  Screen ({ state = MainMenu.empty dim; ops = (module MainMenu) })

type action = 
| NewScreen of screen
| Command of event

let route dim = function
| Menu -> NewScreen (create_main_menu dim)
| Play -> NewScreen (create_play dim)
| Settings -> NewScreen (create_settings dim)
| Quit -> Command `End

let update screen event = 
  match screen with
  | Screen { state; ops = (module Ops) } -> Screen { state = fst @@ Ops.update state event; ops = (module Ops); }

let update dust_state (evt : event) =
  let { current_screen; _ } = Dust.State.extract dust_state in
  let dim = Dust.State.dim dust_state in
  match current_screen with Screen { state; ops = (module Ops) } ->
  let current_screen, command = (match evt with
  | `Route screen -> (
    match route dim screen with
    | NewScreen s -> s, None
    | Command command -> current_screen, Some command)
  | _ ->
    let state, command = Ops.update state evt in
    let current_screen = Screen { state; ops = (module Ops); } in
    current_screen, command)
  in
  let dust_state = { current_screen } |> Dust.State.set dust_state in
  match command with 
  | Some command -> Dust.State.add_command command dust_state, true
  | None -> dust_state, true

let model = 
  { current_screen = (create_main_menu (10, 10)) }

let render dim { current_screen; _ } = 
  match current_screen with Screen { state; ops = (module Ops) } -> Ops.render state dim

let help_text = 
  [ 
    ("↑/↓", "Up/Down");
    ("spc", "Select");
    ("esc", "Back");
  ] |> Common.controls

let render_with_layout d s = render d s |> Common.layout d "Menu" help_text

let () = Dust.run ~model ~render:render_with_layout ~update ()
