module type MinHeapElement = sig
  type 'a t
  val compare : 'a t -> 'a t -> int
  val equal : 'a t -> 'a t -> bool
end

module type MinHeap = sig

  type 'a t

  type 'a elt

  val empty : 'a t

  val insert : 'a elt -> 'a t -> 'a t

  val peek : 'a t -> 'a elt option

  val remove : 'a elt -> 'a t -> 'a t

  val pop_exn : 'a t -> ('a elt * 'a t)

  val pop_opt : 'a t -> ('a elt option * 'a t)

end

let log2 i = 
  let rec test i c =
    if (Int.logand i (Int.lognot Int.one)) = 0 then c
    else
      test (Int.shift_right_logical i 1) c+1
  in
  test i 0

let%test _ =
  let data = [(2, 1); (3, 1); (4, 2); (1, 0); (0, 0); ] in
  List.for_all (fun (i, l) -> log2 i = l) data 

module Make(M : MinHeapElement) : MinHeap with type 'a elt := 'a M.t = struct

  type dir = Left | Right

  let dir_eq d1 d2 = match d1,d2 with
  | Left, Left -> true
  | Right, Right -> true
  | _ -> false

  type 'a elt = 'a M.t
  type 'a node = Leaf | Node of 'a elt * 'a node * 'a node

  (* rep:
      a min heap as a binary tree and a size 

     invariant: 
       size == number of Nodes in the heap
       for every node, the value at the node is the min of itself and
       the values at non-leaf children
   *)
  type 'a t = {
    head : 'a node;
    size : int;
  }

  let empty = { size = 0; head = Leaf }

  let node x = Node (x, Leaf, Leaf)

  let swap x y = 
    if (M.compare x y) > 0 then 
      (y, x) 
    else 
      (x, y)

  let rec set el parent path = 
    match parent, path with
    | _, [] -> Node (el, Leaf, Leaf)
    | Node (v, l, r), Left :: p -> let v', el' = swap v el in Node (v', set el' l p, r)
    | Node (v, l, r), Right :: p -> let v', el' = swap v el in Node (v', l, set el' r p)
    | _ -> failwith "Invariant violated"

  let rec remove_at parent path = 
    match parent, path with
    | Node (v, _, _), [] -> v, Leaf
    | Node (v, l, r), Left :: p -> 
        let res, n = remove_at l p in 
        res, Node (v, n, r)
    | Node (v, l, r), Right :: p ->
        let res, n = remove_at r p in 
        res, Node (v, l, n)
    | _ -> failwith "Invariant violated"

  let rec get parent path = match parent, path with
  | Node (v, _, _), [] -> v
  | Node (v, l, r), Left :: p -> get l p
  | Node (v, l, r), Right :: p -> get r p
  | _ -> failwith "Invariant violated"

  let rec build_path size idx path = 
    let test idx = 
      Int.logand (Int.shift_right_logical size (idx - 1)) Int.one = Int.one
    in
    match idx with
    | idx when idx <= 0 -> []
    | idx -> 
        let dir = if test idx then Right else Left in
        dir :: build_path size (idx-1) path

  let path size = 
    match size with
    | 0 -> []
    | x -> build_path size (log2 size) []

  let rec sift node = 
    match node with
    | Leaf -> node
    | Node (v, l, r) ->
        match l, r with 
        | Node (vl, ll, lr), (Leaf as r) when M.compare vl v < 0 -> Node (vl, sift (Node (v, ll, lr)), r)
        | Node (vl, ll, lr), (Node (vr, rl, rr) as r) when M.compare vl v < 0 && M.compare vl vr <= 0 -> Node (vl, sift (Node (v, ll, lr)), r)
        | l, Node (vr, rl, rr) when M.compare vr v < 0 -> Node (vr, l, sift (Node (v, rl, rr)))
        | Leaf, Leaf -> node
        | Leaf, _ -> failwith "Invariant violated"
        | _, _ -> node


  let insert el { head; size } = 
    let size = Int.succ size in
    let path = path size in
    let head = set el head path in
    { head; size; }

  let peek { head; size; } = 
    match head with
    | Node (v, _, _) -> Some v
    | Leaf -> None

  let replace_at el path head =
    let apply f (v, l, r) dir = match dir with
    | Left -> Node (v, f l, r)
    | Right -> Node (v, l, f r)
    in
    let rec replace_rec el path node = 
      match node, path with
    | Leaf, _ -> failwith "Replace at path must point to node"
    | Node (v, l, r), [] -> sift (Node (el, l, r))
    | Node (v, l, r), dir :: p when M.compare el v < 0 -> apply (replace_rec v p) (el, l, r) dir
    | Node (v, l, r), dir :: p -> apply (replace_rec el p) (v, l, r) dir
    in
  replace_rec el path head

  let path_eq = List.equal dir_eq

  let rec print_path path = match path with
  | [] -> print_endline "done"
  | Left :: t -> print_endline "Left"; print_path t
  | Right :: t -> print_endline "Right"; print_path t

  let remove el ({ head; size } as t) =
    let rec find_path node el path = match node with
    | Leaf -> None
    | Node (v, _, _) when M.equal v el -> Some path
    | Node (v, l, r) -> 
      match find_path l el (Left :: path) with
      | Some p -> Some p
      | None -> 
      match find_path r el (Right :: path) with
      | Some p -> Some p
      | None -> None
    in
    let replace_path = Option.map List.rev (find_path head el []) in
    match replace_path with
    | None -> t
    | Some p ->
        let () = print_endline ("printing size " ^ Int.to_string size) in
        let () = print_endline "printing replace path" in
        let () = print_path p in
        let last = path size in
        let () = print_endline "printing last path" in
        let () = print_path last in
        let el, head = remove_at head last in
        let size = Int.pred size in
        if path_eq last p then
          { head; size; }
        else
          let head = replace_at el p head in
          { head; size; }

  let pop_opt t =
    let { head; size; } = t in
    match head with
    | Leaf -> None, t
    | Node (v, Leaf, Leaf) -> Some v, { head = Leaf; size = 0 }
    | Node (v, l, r) ->
        let res = Some v in
        let path = path size in
        let new_v, head = remove_at head path in
        match head with 
        | Leaf -> res, t
        | Node (v, l, r) -> res, { head = sift (Node (new_v, l, r)); size = Int.pred size }

  let pop_exn t = match pop_opt t with 
  | Some x, t -> x, t
  | None, _-> failwith "Heap is empty"

  (** TEST **)

  let string_of_dir = function
  | Left -> "Left"
  | Right -> "Right"

  let%test _ = 
    let rec compare_paths p1 p2 = match p1, p2 with
    | [], [] -> true
    | Left :: p1', Left :: p2' -> compare_paths p1' p2'
    | Right :: p1', Right :: p2' -> compare_paths p1' p2'
    | _ -> false
    in
    let data = [
      (1, []);
      (2, [Left]);
      (3, [Right]);
      (4, [Left; Left]);
      (5, [Left; Right]);
      (9, [Left; Left; Right]);
      (10, [Left; Right; Left]);
      (15, [Right; Right; Right]);
      (16, [Left; Left; Left; Left]);
      (17, [Left; Left; Left; Right]);
    ]
    in
    List.for_all (fun (s, p) -> path s |> compare_paths p) data

end


(** TEST **) 

module IntHeapElement = struct
  type 'a t = int

  let compare = Int.compare
  let equal = Int.equal
end

let%test _ =
  let open Make(IntHeapElement) in
  match pop_opt empty with
  | (None, _) -> true
  | _ -> false

let%test _ =
  let open Make(IntHeapElement) in
  let t = empty |> insert 2 |> insert 1 |> insert 3 in 
  let other = empty |> insert 1 |> insert 2 |> insert 3 in
  match pop_opt t, pop_opt other with
  | (Some 1, _), (Some 1, _) -> true
  | _ -> false


