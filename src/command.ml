module CommandQueue : sig
  type 'a t
  val empty : 'a t
  val push : 'a -> 'a t -> 'a t
  val pop_opt : 'a t -> 'a option * 'a t
end = struct
  type 'a t = 'a list * 'a list

  let empty = [], []

  let push cmd (head, tail) = (head, cmd :: tail)

  let pop_opt (head, tail) = 
    match head,tail with
    | [], [] -> None, empty
    | h :: [], t -> (Some h, (List.rev t, []))
    | h :: hs, t -> (Some h, (hs, t))
    | [], t -> 
        match List.rev t with
        | h :: hs -> (Some h, (hs, []))
        | [] -> (None, ([], []))
end
