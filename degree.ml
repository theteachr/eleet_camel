type value = { range : (int * int) option; count : int }

module Data = Map.Make (Int)

let build_knowledge nums =
  let aux (idx, data) n =
    let value =
      Data.find_opt n data
      |> Option.value ~default:{ range = Some (idx, idx); count = 0 }
    in
    let left, _ = Option.get value.range in
    let prev_count = value.count in
    ( idx + 1,
      Data.add n { range = Some (left, idx); count = prev_count + 1 } data )
  in
  List.fold_left aux (0, Data.empty) nums |> snd

let print_entry n value =
  let open Printf in
  let range =
    Option.map (fun (s, e) -> sprintf "(%d, %d)" s e) value.range
    |> Option.value ~default:"<>"
  in
  sprintf "%d: %s [%d]" n range value.count |> print_endline

let process knowledge =
  let degree =
    Data.fold (fun _ { count } curr_max -> Int.max curr_max count) knowledge 0
  in
  let most_frequent =
    Data.fold
      (fun num { count } res -> if count = degree then num :: res else res)
      knowledge []
  in
  List.fold_left_map
    (fun acc n ->
      let { range } = Data.find n knowledge in
      let start_idx, end_idx = Option.get range in
      let subarray_len = end_idx - start_idx + 1 in
      (Int.min acc subarray_len, subarray_len))
    Int.max_int most_frequent

let test = [ 1; 2; 2; 1; 3 ]
let test = [ 1; 2; 2; 3; 1; 4; 2 ]
let () = Data.iter print_entry (build_knowledge test)
let solve = test |> build_knowledge |> process
