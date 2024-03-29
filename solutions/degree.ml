open Stdplus

type input = int list

type output = int option

let parse line = line |> String.split_on_char ' ' |> List.map int_of_string

let to_string = function
  | Some solution -> Int.to_string solution
  | None -> "0"

type occurrence =
  { first_at : int
  ; last_at : int
  ; count : int
  }

module Occurrences = Map.Make (Int)

(** [build_occurrences nums] returns the most frequent number in [nums] and a
    `Map` that goes from a number to its `occurrence` holding the following
    information about it:
    - the index at which it *first* occurred
    - the index at which it *last* occurred
    - the number of times it has occurred in [nums] *)
let build_occurrences nums =
  let update_degree_occs (degree, occs) (idx, n) =
    let occ =
      match Occurrences.find_opt n occs with
      | Some ({ count; _ } as v) -> { v with last_at = idx; count = count + 1 }
      | None -> { first_at = idx; last_at = idx; count = 1 }
    in
    (Int.max occ.count degree, Occurrences.add n occ occs)
  in
  List.fold_left update_degree_occs (0, Occurrences.empty) (List.enumerate nums)

let print_entry n value =
  let { first_at; last_at; count } = value in
  let open Printf in
  printf "%d: (first_at: %d, last_at: %d) [%d]\n" n first_at last_at count

let solve test =
  let degree, occurrences = build_occurrences test in
  let occurrence_length (_, { first_at; last_at; count }) =
    if count = degree then Some (last_at - first_at + 1) else None
  in
  let subarray_lens_of_most_frequent =
    Occurrences.bindings occurrences |> List.filter_map occurrence_length
  in
  match subarray_lens_of_most_frequent with
  | first :: rest -> Some (List.fold_left Int.min first rest)
  | [] -> None
