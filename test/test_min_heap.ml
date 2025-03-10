module IntHeapElement = struct
  type 'a t = int

  let compare = Int.compare
  let equal = Int.equal
end

module IntHeap = Min_heap.Make(IntHeapElement)

type heap_op = Insert of int | Pop of int option | Remove of int

let p x = Pop (Some x)
let n = Pop None
let i x = Insert x
let r x = Remove x

let apply t = 
  let open IntHeap in 
  let open Alcotest in
  function
  | Insert x -> t |> insert x
  | Remove x -> t |> remove x
  | Pop s -> 
      let v, t = pop_opt t in 
      match s, v with
      | None, None -> t
      | Some s', None  -> fail @@ Printf.sprintf "Expected pop to have value %d but received None" s'
      | None, Some v' -> fail @@ Printf.sprintf "Expected pop to return None but received value %d" v'
      | Some s', Some v' -> (check int) "heap pops min" s' v'; t

let basic = 
  [
    [i 1; p 1; n; i 2; i 4; i 5; i (-1); p (-1); p 2; i 1; p 1; p 4; p 5; n];
    [n; n; i 10; i 100; i 1000; i (-256); i 0; p (-256); i 999999; p 0; p 10;];
    [i 4; i 3; i 2; i 1; i 0; i (-1); i 10; p (-1); p 0; p 1; p 2; p 3; p 4; p 10; n];
  ]

let complex =
  [
    [i 1; r 1; n; i 2; i 3; i 99; i 0; r 0; p 2; r 3; p 99; n];
    [r 1; n; r 1; n; i 1; r 0; p 1; i 10; r 9; p 10; i 0; r 0; n];
    [i 4; i 1; i 2; i 5; i 6; i 33; r 4; p 1; p 2; p 5; p 6; p 33; n];
    [i 1; i 11; i 5; i 17; i 12; i 7; i 6; i 17; i 18; i 14; i 13; i 7;
     r 12; p 1; p 5; p 6; p 7; p 7; p 11; p 13; p 14; p 17; p 17; p 18; n];
    [i 12; i 13; i 4; i 5; i 99; i 100; i (-10); i 0; i 100; i 10; i 10;
     i 1; i 2; i 3; i 10; i 9; i 8; i 7; i 6; i 5; i 9; r 13; r 12; r 10;
     p (-10); p 0; p 1; p 2; p 3; p 4; p 5; p 5; p 6; p 7; p 8; p 9; p 9;
     p 10; p 10; p 99; p 100; p 100; n]
  ]

let test_heap ops () =
  let open IntHeap in
  ignore @@ List.fold_left (fun heap op -> apply heap op) empty ops

let () =
  let open Alcotest in
  run "Int Min Heap" [
      "basic", List.mapi (fun idx ops -> test_case ("heap_" ^ Int.to_string idx) `Quick (test_heap ops)) basic;
      "complex", List.mapi (fun idx ops -> test_case ("heap_" ^ Int.to_string idx) `Quick (test_heap ops)) complex;
    ]

