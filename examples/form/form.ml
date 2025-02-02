open Dust
open Notty
open Styles


type form = {
  focused : bool;
  name : string;
  width : int;
  value : string;
}

type button = {
  focused : bool;
  name : string;
  on_press : unit -> event;
}


let render_form { focused; name; width; value; }= 
  let attr = if focused then A.(bg (gray 10) ++ fg lightwhite) else A.empty in
  let value = I.string attr value |> Layout.pad ~r:(width - String.length value) attr in
  let value = Styles.Outline.outline ~border:`Square A.empty value in
  let name = I.string A.empty name |> I.pad ~l:2 in
  Layout.flex_v ~align:`Left A.empty [ name; value]

let render_button { name; focused; _ } =
  let attr = if focused then A.(bg (gray 10) ++ fg lightwhite) else A.empty in
  let name = I.string attr name in
  Styles.Outline.outline ~border:`Square A.empty name


type state = {
  name : form; 
  age : form;
  button : button;
}

let render _ state = 
  let name = render_form state.name in
  let age = render_form state.age in
  let button = render_button state.button in
  Layout.flex_h ~align:`Bottom ~gap:4 A.empty [name; age; button]


let state = {
  name = {
    focused = true;
    width = 20;
    name = "Name";
    value = "";
  };

  age = {
    focused = false;
    width = 10;
    name = "Age";
    value = "";
  };

  button = {
    focused = false;
    name = "Submit";
    on_press = fun () -> `End;
  };
}



let update_form (f: form) evt =
  let handle_char f c =
    let value = Printf.sprintf "%s%c" f.value c in
    { f with value }
  in
  let handle_backspace f =
    let len = String.length f.value in
    if len = 0 then f
    else
      let value = String.sub f.value 0 (len - 1) in
      { f with value }
  in
  match evt with
  | `Key (`ASCII c, _) -> handle_char f c
  | `Key (`Backspace, _) -> handle_backspace f
  | _ -> f



let init = State.return state



let update (state : (state, Dust.event) State.t) (evt : Dust.event) =
  let s = State.get state in
  let none s = (s, None) in
  let some s e = (s, Some e) in
  let s, cmd = match evt with
  | `Key (`Tab, _) -> 
    (match s with
    | { name = { focused = true; _ } as n; age; _ } ->  none { s with name = { n with focused = false }; age = { age with focused = true } }
    | { age = { focused = true; _ } as a; button; _ } ->  none { s with button = { button with focused = true }; age = { a with focused = false } }
    | { button = { focused = true; _ } as b; name; _ } -> none { s with button = { b with focused = false }; name = { name with focused = true } }
    | _ -> none s)
  | `Key (`ASCII ' ', _) when s.button.focused = true -> some s (s.button.on_press ())
  | _ ->
    match s with
    | { name = { focused = true; _ }; _} ->  none { s with name = update_form s.name evt }
    | { age = { focused = true; _ }; _} ->  none { s with age = update_form s.age evt }
    | _ -> none s
  in

  let state = state |> State.map (fun _ -> s) in

  let state = match cmd with 
  | None -> state
  | Some c -> state |> State.add_command c
  in
  state, true



let render_with_layout d s = render d s |> Common.layout d "Form" I.empty


let () = Dust.run ~init ~update ~render:render_with_layout ()



