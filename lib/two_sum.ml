type input = int list * int
type output = int * int

module Index_map = Map.Make (Int)

let parse lines =
  let to_nums line = String.split_on_char ' ' line |> List.map int_of_string in
  match String.split_on_char '\n' lines with
  | [ nums_line; target ] -> (to_nums nums_line, int_of_string target)
  | _ -> failwith "Malformed input"

let to_string (f, s) = Printf.sprintf "%d %d" f s

let solve (nums, target) =
  let rec aux acc = function
    | [] -> failwith "The problem statement lied"
    | (i, n) :: t -> begin
        match Index_map.find_opt (n - target |> abs) acc with
        | Some i' -> (i', i) (* TODO: Make sure i' < i *)
        | _ -> aux (Index_map.add n i acc) t
    end
  in
  aux Index_map.empty (nums |> List.mapi (fun i n -> (i, n)))
