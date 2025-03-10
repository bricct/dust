type 'a timer

type 'a t

val add_timer : 'a timer -> 'a t -> 'a t
val remove_timer : 'a timer -> 'a t -> 'a t

val get_promise : 'a t -> 'a Lwt.t option
