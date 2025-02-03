open Notty

let outline ?(border = `Square) attr i  =
  let (h, v, a, b, c, d) = match border with
    | `Square -> (0x2500, 0x2502, 0x250c, 0x2510, 0x2514, 0x2518)
    | `Round -> (0x2500, 0x2502, 0x256d, 0x256e, 0x2570, 0x256f)
    | `Double -> (0x2550, 0x2551, 0x2554, 0x2557, 0x255d, 0x255a)
    | `Open -> (0x2500, 0x0020, 0x2500, 0x2500, 0x2500, 0x2500)
  in
  let (width, height) = I.(width i, height i) in
  let chr x = I.uchar attr (Uchar.of_int x) 1 1 in
  let hbar  = I.uchar attr (Uchar.of_int h) (width+2) 1 in
  let vbar  = I.uchar attr (Uchar.of_int v) 1 height in
  let i = Layout.pad ~l:1 ~r:1 attr i in
  Layout.grid [ [chr a; hbar; chr b]; [vbar; i; vbar]; [chr c; hbar; chr d] ]

