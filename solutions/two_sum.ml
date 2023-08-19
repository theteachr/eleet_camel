open Stdplus

type input = int list * int

type output = int * int

module Index_map = Map.Make (Int)

let parse lines =
  let to_nums line = String.split_on_char ' ' line |> List.map int_of_string in
  match String.split_on_char '\n' lines with
  | [ nums_line; target ] -> (to_nums nums_line, int_of_string target)
  | _ -> failwith "Malformed input"

let to_string (f, s) =
  let small, big = if f < s then (f, s) else (s, f) in
  Printf.sprintf "%d %d" small big

let solve (nums, target) =
  let rec find_index_pair num_idx_map = function
    | (i, n) :: t -> begin
        match Index_map.find_opt (n - target |> abs) num_idx_map with
        | Some i' -> (i', i)
        | _ -> find_index_pair (Index_map.add n i num_idx_map) t
      end
    | [] -> failwith "The problem statement lied"
  in
  find_index_pair Index_map.empty (List.enumerate nums)
