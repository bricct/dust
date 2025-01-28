open Dust
open Notty
open Styles

let layout (width, height) name i =
  let g = A.(bg (gray 4)) in
  let close = I.string A.(fg (gray 12) ++ g) "(Ctrl-Q): Exit" in
  let name = I.string A.(fg white ++ st bold ++ g) name in
  let header = Layout.justify ~attr:g width [close; name; I.void 1 1] in
  I.( header <-> i )

