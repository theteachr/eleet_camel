type occurrence = { first_at : int; length : int; count : int }

module Occurrences = Map.Make (Int)

let build_knowledge nums =
  let indexed = List.mapi (fun i n -> (i, n)) nums in
  let update_degree_and_occs (degree, occs) (idx, n) =
    let degree, occ =
      match Occurrences.find_opt n occs with
      | Some ({ count; first_at } as v) ->
          let count = count + 1 in
          (Int.max degree count, { v with length = idx - first_at + 1; count })
      | None -> (degree, { first_at = idx; length = 1; count = 1 })
    in
    (degree, Occurrences.add n occ occs)
  in
  List.fold_left update_degree_and_occs (0, Occurrences.empty) indexed

let print_entry n value =
  let { first_at; length; count } = value in
  Printf.sprintf "%d: (first_at: %d, length: %d) [%d]" n first_at length count
  |> print_endline

let process (degree, knowledge) =
  let cons_if_degree n { count; length } acc =
    if count = degree then length :: acc else acc
  in
  let subarray_lengths = Occurrences.fold cons_if_degree knowledge [] in
  match subarray_lengths with
  | first :: rest -> Some (List.fold_left Int.min first rest)
  | [] -> None

let test = [ 1; 2; 2; 1; 3 ]
let () = Occurrences.iter print_entry (build_knowledge test |> snd)
let solve = test |> build_knowledge |> process