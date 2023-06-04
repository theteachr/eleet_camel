type occurrence = { first_at : int; last_at : int; count : int }

module Occurrences = Map.Make (Int)

let build_knowledge nums =
  let indexed = List.mapi (fun i n -> (i, n)) nums in
  let update_degree_and_occs (degree, occs) (idx, n) =
    let degree, occ =
      match Occurrences.find_opt n occs with
      | Some ({ count; _ } as v) ->
          let count = count + 1 in
          (Int.max degree count, { v with last_at = idx; count })
      | None -> (degree, { first_at = idx; last_at = idx; count = 1 })
    in
    (degree, Occurrences.add n occ occs)
  in
  List.fold_left update_degree_and_occs (0, Occurrences.empty) indexed

let print_entry n value =
  let { first_at; last_at; count } = value in
  Printf.sprintf "%d: (first_at: %d, last_at: %d) [%d]" n first_at last_at count
  |> print_endline

let process (degree, knowledge) =
  let occurrence_length (_, { first_at; last_at; count }) =
    if count = degree then Some (last_at - first_at + 1) else None
  in
  let subarray_lens_of_most_frequent =
    Occurrences.bindings knowledge |> List.filter_map occurrence_length
  in
  match subarray_lens_of_most_frequent with
  | first :: rest -> Some (List.fold_left Int.min first rest)
  | [] -> None

let tests = [ [ 1; 2; 2; 1; 3 ]; [ 1; 2; 2; 3; 1; 4; 2 ] ]

let () =
  let print_occs test =
    Occurrences.iter print_entry (build_knowledge test |> snd)
  in
  tests |> List.iter print_occs

let solutions =
  List.rev_map (fun test -> test |> build_knowledge |> process) tests
