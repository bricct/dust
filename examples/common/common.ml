open Dust
open Notty
open Styles

let layout (width, height) name i =
  let g = A.(bg (gray 4)) in
  let close = 
    I.( I.string A.(fg (gray 12) ++ g) "(Ctrl-Q): "
    <|> I.string A.(fg white ++ g) "Exit")
  in
  let name = I.string A.(fg white ++ st bold ++ g) name in
  let lspace = (width / 2) - (I.width name / 2) - (I.width close) in
  let rspace = ((width - lspace) - (I.width name / 2)) + (I.width name mod 2) in
  let header = I.( close <|> Layout.pad ~l:lspace ~r:rspace g name ) in
  let i = Layout.box ~width A.empty i |> Layout.pad ~t:2 A.empty in
  I.( header <-> i )

