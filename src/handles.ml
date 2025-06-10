module Handle : sig
  include Map.OrderedType
  val of_string : string -> t
  val to_string : t -> string
  val equal : t -> t -> bool
end = struct
  type t = string
  let of_string = Fun.id
  let to_string = Fun.id
  let compare = String.compare
  let equal = String.equal
end

type background_work = Task | Stream | Timer


type task_info = background_work * Handle.t

module TimerSet = struct
  module HandleSet = Set.Make(Handle)

  type t = HandleSet.t

  let empty = HandleSet.empty

  let get_handle str t =
    let handle = Handle.of_string str in
    match HandleSet.find_opt handle t with 
    | None -> failwith ("Get: Handle not found " ^ str)
    | Some _ -> handle

  let make_handle str t = 
    let handle = Handle.of_string str in
    match HandleSet.find_opt handle t with 
    | None -> handle
    | Some _ -> failwith ("Make: Handle already registered: " ^ str)

  let add handle t =
    match HandleSet.find_opt handle t with 
    | None -> HandleSet.add handle t
    | Some _ -> failwith ("Add: Handle already registered: " ^ (handle |> Handle.to_string))

  let remove handle t =
    match HandleSet.find_opt handle t with
    | None -> t
    | Some _ -> HandleSet.remove handle t

end

module TaskMap = struct
  module HandleMap = Map.Make(Handle)

  type 'a t = 'a HandleMap.t

  let empty = HandleMap.empty

  let get_handle_opt str t = 
    let handle = Handle.of_string str in
    Option.map (fun _ -> handle) @@ HandleMap.find_opt handle t

  let get_handle str t =
    match get_handle_opt str t with
    | None -> failwith ("Get: Handle not found " ^ str)
    | Some handle -> handle

  let make_handle str t = 
    let handle = Handle.of_string str in
    match HandleMap.find_opt handle t with 
    | None -> handle
    | Some _ -> failwith ("Make: Handle already registered: " ^ str)

  let add handle task t = 
    match HandleMap.find_opt handle t with 
    | None -> HandleMap.add handle task t
    | Some _ -> failwith ("Add: Handle already registered: " ^ (handle |> Handle.to_string))

  let remove ?on_remove handle t =
    match HandleMap.find_opt handle t with
    | None -> t
    | Some task -> 
        let () = Option.iter (fun f -> f task) on_remove in 
        HandleMap.remove handle t

  let find handle t = HandleMap.find handle t

  let update handle task t = HandleMap.update handle task t

  let list_map f t = HandleMap.fold (fun _ a acc -> f a :: acc) t []
end
