open Dust
open Notty
open Styles

let layout (width, height) name help i =
  let g = A.(bg (gray 4)) in
  let close = 
    I.( I.string A.(fg (gray 12) ++ g) "(Ctrl-Q): "
    <|> I.string A.(fg white ++ g) "exit")
  in
  let name = I.string A.(fg white ++ st bold ++ g) name in
  let lspace = ((width / 2) - (I.width name / 2)) - (I.width close) in
  let rspace = (((width / 2) + width mod 2) - ((I.width name / 2))) in
  let header = Layout.pad ~l:lspace ~r:rspace ~attr:g name in
  let header = I.( close <|> header ) in
  Layout.flex_v ~gap:2 ~align:`Middle [header; i; help ]


let controls controls = 
  let make_control (k, v) = 
    let fmt = Printf.sprintf "(%s): " k in
    let keys = I.string A.(bg (gray 3) ++ fg (gray 10)) fmt in
    let text = I.string A.(bg (gray 3)) v in
    I.( keys <|> text )
  in
  let imgs = 
    List.map make_control controls
  in
  Layout.flex_h ~gap:4 imgs
