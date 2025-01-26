open Notty

val grid : image list list -> image

val center : int -> int -> image -> image

val block : attr -> int -> int -> image

val pad : ?l:int -> ?t:int -> ?r:int -> ?b:int -> attr -> image -> image

val box : ?width:int -> ?height:int -> ?v_align:[ `Top | `Middle | `Bottom ] -> ?h_align:[ `Left | `Middle | `Right ] -> attr -> image -> image

val flex_v : ?gap:int -> ?align:[ `Left | `Middle | `Right] -> attr -> image list -> image

val flex_h : ?gap:int -> ?align:[ `Top | `Middle | `Bottom] -> attr -> image list -> image
