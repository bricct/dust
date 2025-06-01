open Notty

type event = [`End
       | `Key of Notty.Unescape.key
       | `Mouse of Notty.Unescape.mouse
       | `Paste of Notty.Unescape.paste
       | `Resize of int * int ]

module State : sig

  type ('a, 'b) t constraint 'b = [> event]

  val map : ('a -> 'c) -> ('a, 'b) t -> ('c, 'b) t 

  val extract : ('a, 'b) t -> 'a
  val extend : (('a, 'b) t -> 'c) -> ('a, 'b) t -> ('c, 'b) t

  val dim : ('a, 'b) t -> int * int

  val set : ('a, 'b) t -> 'a -> ('a, 'b) t

  val add_command : 'b -> ('a, 'b) t -> ('a, 'b) t
  val add_task : (unit -> 'b Lwt.t) -> string -> ('a, 'b) t -> ('a, 'b) t
  val add_stream : (unit -> 'b Lwt.t) -> string -> ('a, 'b) t -> ('a, 'b) t 
  val add_timer : ?iters:int -> string -> ms:int -> event:'b -> ('a, 'b) t -> ('a, 'b) t

  val remove_timer : string -> ('a, 'b) t -> ('a, 'b) t
  val remove_task : string -> ('a, 'b) t -> ('a, 'b) t
  val try_remove_task : string -> ('a, 'b) t -> ('a, 'b) t
  val remove_stream : string -> ('a, 'b) t -> ('a, 'b) t

end

type ('a, 'b) state = ('a, 'b) State.t

val run : render:(int * int -> 'a -> image) ->
model:'a ->
update:(('a, 'b) state -> 'b -> ('a, 'b) state * bool) ->
?init:(('a, 'b) state -> ('a, 'b) state) ->
unit ->
unit

