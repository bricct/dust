open Notty

type event = [`End
       | `Key of Notty.Unescape.key
       | `Mouse of Notty.Unescape.mouse
       | `Paste of Notty.Unescape.paste
       | `Resize of int * int ]

module State : sig


  type ('a, 'b) t constraint 'b = [> event]

  val map : ('a -> 'a) -> ('a, 'b) t -> ('a, 'b) t 
  val return : 'a -> ('a, 'b) t

  val get : ('a, 'b) t -> 'a

  val add_task : (unit -> 'b Lwt.t) -> ('a, 'b) t -> ('a, 'b) t
  val add_stream : (unit -> 'b Lwt.t) -> ('a, 'b) t -> ('a, 'b) t 

end

type ('a, 'b) state = ('a, 'b) State.t


val run : render:(int * int -> 'a -> image) ->
init:('a, 'b) state ->
update:(('a, 'b) state -> 'b -> ('a, 'b) state * bool) ->
unit ->
unit

