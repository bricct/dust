open Notty

type ('a, 'b) state

val make_state : 'a -> ('a, 'b) state

val map : ('a, 'b) state -> ('a -> 'a) -> ('a, 'b) state

val add_task : (unit -> 'b Lwt.t) -> ('a, 'b) state -> ('a, 'b) state

val add_timer : float -> 'b -> ('a, 'b) state -> ('a, 'b) state

val run : render:(int * int -> 'a -> image) ->
  init:('a, [> `End 
     |  `Key of Unescape.key
     | `Mouse of Unescape.mouse
     | `Paste of Unescape.paste
     | `Resize of int * int 
    ]
   as
   'b) state ->
update:
  (('a, 'b) state ->
  'b ->
  ('a -> 'a) * bool) -> unit -> unit

