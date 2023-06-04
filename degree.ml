type indexed_count = { first_at : int; last_at : int; count : int }

module IndexedCount = Map.Make (Int)

let build_knowledge nums =
  let indexed = List.mapi (fun i n -> (i, n)) nums in
  let aux (curr_max, data) (idx, n) =
    let value =
      match IndexedCount.find_opt n data with
      | Some v -> v
      | None -> { first_at = idx; last_at = idx; count = 0 }
    in
    let new_count = value.count + 1 in
    ( Int.max curr_max new_count,
      IndexedCount.add n { value with last_at = idx; count = new_count } data )
  in
  List.fold_left aux (0, IndexedCount.empty) indexed |> Option.some

let print_entry n value =
  Printf.sprintf "%d: (first_at: %d, last_at: %d) [%d]" n value.first_at
    value.last_at value.count
  |> print_endline

let process (degree, knowledge) =
  Printf.sprintf "Degree: %d" degree |> print_endline;
  let most_frequent =
    IndexedCount.fold
      (fun num { count } res -> if count = degree then num :: res else res)
      knowledge []
  in
  match most_frequent with
  | first :: rest ->
      List.fold_left_map
        (fun acc n ->
          let { first_at; last_at } = IndexedCount.find n knowledge in
          let subarray_len = last_at - first_at + 1 in
          (Int.min acc subarray_len, subarray_len))
        first most_frequent
      |> Option.some
  | [] -> None

let test = [ 1; 2; 2; 1; 3 ]

let () =
  match build_knowledge test with
  | Some (_, data) -> IndexedCount.iter print_entry data
  | None -> print_endline "¯\_(ツ)_/¯"

let solve =
  let res = test |> build_knowledge in
  Option.bind res process
