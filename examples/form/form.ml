open Dust
open Notty
open Styles

type input = {
  name: string;
  value: string;
}

type button = {
  name : string;
  on_press : unit -> event;
}

type align = [`Left | `Middle | `Right]

type formatting = {
  align : align;
  width : int;
}

type field = Input of input | Button of button

type form_field = {
  field : field;
  format : formatting;
}

type form = {
  prev : form_field list;
  current : form_field option;
  next : form_field list;
  format : formatting;
  gap : int;
}

let create_form ?(gap = 0) ?(align = `Left) fields width = {
  prev = [];
  current = None;
  next = fields;
  format = {
    width;
    align;
  };
  gap
}

let focus_next form = 
  let stash prev = match form.current with
  | None -> prev
  | Some field -> field :: prev
  in
  match form.next with
  | [] -> form
  | field :: fields -> 
      let prev = stash form.prev in
      let current = Some field in
      let next = fields in
    { form with prev; current; next; }

let focus_prev form = 
  let stash next = match form.current with
  | None -> next
  | Some field -> field :: next
  in
  match form.prev with
  | [] -> form
  | field :: fields -> 
      let next = stash form.next in
      let current = Some field in
      let prev = fields in
    { form with prev; current; next; }

let render_input { name; value; } focused width = 
  let attr = if focused then A.(bg (gray 10) ++ fg lightwhite) else A.empty in
  let value = I.string attr value |> Layout.pad ~r:((width - 4) - (String.length value)) ~attr:attr in
  let value = Styles.Outline.outline ~border:`Round value in
  let name = 
    I.string A.(st bold) name 
    |> I.pad ~l:1
  in
  Layout.flex_v ~align:`Left [ name; value]

let render_button { name; _ } focused width =
  let attr = if focused then A.(bg (gray 10) ++ fg lightwhite) else A.empty in
  let name = 
    I.string attr name 
    |> Layout.box ~width:(width - 4) ~height:1 ~h_align:`Middle ~attr:attr 
  in
  Styles.Outline.outline ~border:`Round name

let render_form f = 
  let { prev; current; next; format; gap; } = f in
  let { width; align } = format in

  let render_form_field form_field focused w = 
    match form_field.field with
    | Input input -> render_input input focused w
    | Button button -> render_button button focused w
  in

  let w = width in
  let add_with_overflow (image, current_row) field_image = 
    let remaining = w - (I.width current_row) in
    match remaining - (I.width field_image) with
    | x when x < 0 -> I.( image <-> I.void w 1 <-> current_row ), field_image
    | x when (I.width current_row = 0) -> image, field_image
    | x -> image, I.( current_row <|> (I.void gap 1) <|> field_image)
  in

  let prev = List.rev_map (fun f -> render_form_field f false f.format.width) prev in
  let next = List.map (fun f -> render_form_field f false f.format.width) next in
  let img = List.fold_left add_with_overflow (I.empty, I.empty) prev in
  let img = Option.fold ~none:img ~some:(fun c -> add_with_overflow img (render_form_field c true c.format.width)) current in
  let (i, last_row) = List.fold_left add_with_overflow img next in
  I.( i <-> I.void w 2 <-> last_row)


type state = {
  form : form;
}

let render _ state = 
  render_form state.form
  |> I.pad ~t:1

let name = {
  field = Input ({
    name = "Name";
    value = "";
  });
  format = {
    width = 20;
    align = `Left
  }
}

let age = {
  field = Input ({ 
    name = "Age";
    value = "";
  });
  format = {
    width = 8;
    align = `Left
  }
}

let email = {
  field = Input ({
    name = "Email";
    value = "";
  });
  format = {
    width = 30;
    align = `Left;
  }
}

let button = {
  field = Button ({
    name = "Submit";
    on_press = fun () -> `End;
  });
  format = {
    width = 30;
    align = `Middle
  }
}

let fields = [name; age; email; button]

let form = create_form ~gap:2 fields 30

let state = {
  form
}

let update_input f evt =
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
  | `Key (`ASCII c, _) -> handle_char f c, None
  | `Key (`Backspace, _) -> handle_backspace f, None
  | _ -> f, None

let update_button b evt =
  match evt with
  | `Key (`ASCII ' ', _) -> b,  Some (b.on_press ())
  | _ -> b, None

let update_form form evt = 
  let update_field f evt = match f.field with
  | Input input -> 
      let i, cmd = update_input input evt in
      Input i, cmd
  | Button button -> 
      let b, cmd = update_button button evt in
      Button b, cmd
  in
  let handle_keys current = match current with
  | None -> (form, None)
  | Some c -> 
      let field, cmd = update_field c evt in
      let current = Some { c with field = field } in
      ({ form with current = current }, cmd)
  in
  match evt with
  | `Key (`Tab, []) -> (focus_next form, None)
  | `Key (`Tab, [`Shift;]) -> (focus_prev form, None)
  | _ -> handle_keys form.current

let init = state

let update (state : (state, Dust.event) State.t) (evt : Dust.event) =
  let s = State.extract state in
  let form, cmd = update_form s.form evt in
  let state = state |> State.map (fun _ -> { form }) in

  let state = match cmd with 
  | None -> state
  | Some c -> state |> State.add_command c
  in
  state, true


let help = 
  let controls = [
    ("Tab", "next");
    ("Shift-Tab", "prev");
    ("Space", "enter");
  ] in
  Common.controls controls

let render_with_layout d s = render d s |> Common.layout d "Form" help

let () = Dust.run ~model:state ~update ~render:render_with_layout ()
