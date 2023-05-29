type indexed_count = { first_at : int; last_at : int; count : int }

module Data = Map.Make (Int)

let build_knowledge nums =
  let aux (idx, data) n =
    let value =
      match Data.find_opt n data with
      | Some v -> v
      | None -> { first_at = idx; last_at = idx; count = 0 }
    in
    let prev_count = value.count in
    ( idx + 1,
      Data.add n { value with last_at = idx; count = prev_count + 1 } data )
  in
  List.fold_left aux (0, Data.empty) nums |> snd

let print_entry n value =
  Printf.sprintf "%d: (first_at: %d, last_at: %d) [%d]" n value.first_at
    value.last_at value.count
  |> print_endline

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
      let { first_at; last_at } = Data.find n knowledge in
      let subarray_len = last_at - first_at + 1 in
      (Int.min acc subarray_len, subarray_len))
    Int.max_int most_frequent

let test = [ 1; 2; 2; 1; 3 ]
let test = [ 1; 2; 2; 3; 1; 4; 2 ]
let () = Data.iter print_entry (build_knowledge test)
let solve = test |> build_knowledge |> process
