open Lwt
open Dust
open Notty


type secret_string = 
{ 
  id : int;
  value : string;
  counter : int;
  revealed : int;
  len : int;
}

let make_secret id s =
{
  id;
  value = s;
  counter = 0;
  revealed = 0;
  len = String.length s
}

type state =
{
  string_count : int;
  strings : secret_string list
}

let empty = {
  string_count = 0;
  strings = []
}

let add_secret_string s state = 
  let sec = make_secret state.string_count s in
  { string_count = state.string_count + 1; strings = sec :: state.strings }

type event = [`Start of int | `Animate of int | Dust.event]

let die () = Lwt_unix.sleep 2. >|= fun _ -> `End
let start_reveal id () = Lwt_unix.sleep 0.03 >|= fun _ -> `Start id
let wait e () = Lwt_unix.sleep 0.2 >|= fun _ -> e
let animate id () = Lwt_unix.sleep 0.03 >|= fun _ -> `Animate id

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

let init : (state, event) State.t = 
  let state = 
    empty 
    |> add_secret_string "Now i am become death" 
    |> add_secret_string "destroyer of worlds ..."
  in
  let dust_state = State.return { state with strings = List.rev state.strings } in
  if state.string_count > 0 then
    dust_state |> State.add_task (wait (`Start 0))
  else
    dust_state



let render_secret (width, _) secret =
  let s = obscure secret.value secret.revealed in
  let decrypted = String.sub s 0 secret.revealed in
  let encrypted = String.sub s secret.revealed (secret.len - secret.revealed) in
  let dec_img = I.string A.(fg white) decrypted in
  let enc_img = I.string A.(fg white) encrypted in
  let img = I.(dec_img <|> enc_img) in
  let s_len = String.length s in
  I.pad ~l: ((width / 2) - (s_len / 2)) ~t:2 img

let render dim state = 
  List.map (fun s -> render_secret dim s) state.strings |> I.vcat


let update (state: (state, event) State.t) (evt: event) = 
  match evt with
  | `Start id -> state 
    |> State.add_task (animate id), true
  | `Animate id ->
      let s = State.get state in
      let secret = List.find (fun sec -> sec.id = id) s.strings in
      if secret.revealed >= secret.len then
        if id = (s.string_count - 1) then
          state |> State.add_task die, true
        else
          state |> State.add_task (start_reveal (id + 1)), true
      else
        state |> State.map (fun (s : state) -> 
          let counter' = secret.counter mod 3 in
          let secret = 
            if counter' = 2 then 
              { secret with counter = 0; revealed = secret.revealed + 1 }
            else 
              { secret with counter = secret.counter + 1 }
          in
          let strings = (List.fold_left (fun acc s -> 
              if s.id = id then 
                secret :: acc
              else
                s :: acc) [] s.strings |> List.rev)
          in
          { s with strings })
        |> State.add_task (animate id), true
  | _ -> state, false

let () = Dust.run ~init ~render ~update ()
