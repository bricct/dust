open Lwt
open Dust
open Notty

module IntMap = Map.Make(Int)

type secret_string =
{ 
  render : string;
  value : string;
  counter : int;
  revealed : int;
  len : int;
}

let chars = 
  let chr_list = 
    [
      'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm';
      'n'; 'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z'
    ]
  in
  let num_list =
    ['1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; '0']
  in
  let sym_list =
    ['/'; '$'; '%'; '@'; '*'; '-'; '+'; '#'; '!'; '>'; ']'; '{' ] 
  in
  Array.of_list (chr_list @ num_list @ sym_list)

let chr_length = Array.length chars

let random_char () = 
  let chr_idx = Random.int chr_length in
  let upper = Random.float 1.0 > 0.5 in
  let c = chars.(chr_idx) in
  if upper then Char.uppercase_ascii c else c

let obscure value revealed = 
  let f idx c = 
    if idx < revealed then c
    else random_char ()
  in
  String.mapi f value

let make_secret s = 
{
  render = obscure s 0;
  value = s;
  counter = 0;
  revealed = 0;
  len = String.length s
}

type state =
{
  debug : bool;
  finished_strings : int;
  string_count : int;
  strings : secret_string IntMap.t
}

let empty = {
  debug = false;
  finished_strings = 0;
  string_count = 0;
  strings = IntMap.empty
}

let add_secret_string s state = 
  let sec = make_secret s in
  { state with string_count = state.string_count + 1; strings = IntMap.add state.string_count sec state.strings }

type event = [`Start of int | `Animate of int | `Finished of int | Dust.event]

let die_ms = 4000
let start_ms = 200
let animate_ms = 16

let die = "die"
let start id = "start_" ^ (Int.to_string id)
let animate id = "animate_" ^ (Int.to_string id)

let fire_once id event ms = State.add_timer ~iters:1 ~ms ~event id

let strings = [
  "Lorem ipsum odor amet, consectetuer adipiscing elit.";
  "Integer potenti urna consequat posuere habitasse interdum.";
  "Dis convallis integer consequat nulla accumsan tincidunt.";
  "Suscipit ullamcorper feugiat bibendum sit vulputate condimentum; egestas elit.";
]

let model = 
  List.fold_left (fun s secret -> (s |> add_secret_string secret)) empty strings


let init : ((state, event) State.t -> (state, event) State.t) = fun dust_state ->
  fst @@ List.fold_left (fun (s, i) _ -> (s |> fire_once (start i) (`Start i) start_ms), i + 1) (dust_state, 0) strings

let render_secret (width, _) secret =
  let img = I.string A.(fg white) secret.render in
  I.pad ~t:2 img

let render_debug dim state = 
  let fmt n v = I.pad ~t:1 ~b:1 ~r:1 ~l:1 @@ I.string A.(fg white) (n ^ ": " ^ v) in
  let fmti n i = fmt n (Int.to_string i) in
  let c_img = fmti "count" state.string_count in
  let f_img = fmti "finished" state.finished_strings in
  let render_secret_debug dim secret =
    let { render; value; counter; revealed; len } = secret in
    let r_img = fmt "render" render in
    let v_img = fmt "value" value in
    let c_img = fmti "counter" counter in
    let rev_img = fmti "revealed" revealed in
    let len_img = fmti "len" len in
    let nums = I.hcat [c_img; rev_img; len_img] in
    I.vcat [r_img; v_img; nums] |> I.pad ~t:1
  in
  let secrets = IntMap.fold (fun _ s l -> (render_secret_debug dim s) :: l) state.strings [] |> List.rev |> I.vcat in
  I.pad ~t:2 @@ I.vcat [c_img; f_img; secrets]

let render dim state = 
   if state.debug then
     render_debug dim state
   else
    IntMap.fold (fun _ s l -> (render_secret dim s) :: l) state.strings [] |> List.rev |> I.vcat

let update (state: (state, event) State.t) (evt: event) = 
  match evt with
  | `Key (`ASCII '\\', []) -> state |> State.map (fun s -> { s with debug = not s.debug }), true
  | `Start id -> state 
    |> fire_once (animate id) (`Animate id) animate_ms, true
  | `Animate id ->
      let s = State.extract state in
      let secret = IntMap.find id s.strings in
      if secret.revealed >= secret.len then
          state |> State.add_command (`Finished id), true 
      else
        state |> State.map (fun (s : state) -> 
          let counter' = secret.counter mod 3 in
          let secret = 
            if counter' = 2 then 
              let revealed = secret.revealed + 1 in
              { secret with counter = 0; revealed; render = obscure secret.value revealed }
            else 
              { secret with counter = secret.counter + 1 }
          in
          let strings = IntMap.update id (fun s -> Option.map (fun _ -> secret) s) s.strings in 
          { s with strings })
        |> fire_once (animate id) (`Animate id) animate_ms, true
  | `Finished _ -> 
      let state = state |> State.map (fun s -> {s with finished_strings = s.finished_strings + 1 }) in
      let s = State.extract state in
      if s.finished_strings = s.string_count then
        state |> fire_once die `End die_ms, true
      else
        state, true
  | _ -> state, false

let render_with_layout d s = render d s |> Common.layout d "Secret" I.empty

let () = Dust.run ~model ~init ~render:render_with_layout ~update ()
