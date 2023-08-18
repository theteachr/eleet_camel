open Infix

type 'a t =
  { items : 'a array array
  ; dimensions : int * int
  }

let of_list l =
  let items = Array.of_list l |> Array.map Array.of_list in
  let num_rows = List.length l in
  let num_cols = try List.(length (hd l)) with Failure _ -> 0 in
  { items; dimensions = (num_rows, num_cols) }

let at (row, col) m =
  try Some m.items.(row).(col) with Invalid_argument _ -> None

let at_exn (row, col) m = m.items.(row).(col)

let dimensions m = m.dimensions

let print_row (row : string array) =
  String.concat " " (Array.to_list row) |> print_endline

let map f m = { m with items = Array.map (Array.map f) m.items }

let print m = Array.iter print_row m.items

let update (row, col) value m =
  m.items.(row).(col) <- value

let get_index value row : int option =
  row
  |> Array.mapi (fun i v -> (i, v))
  |> Array.find_map (fun (i, v) -> if value = v then Some i else None)

let get_index_all value row : int list =
  let rec get_indices found = function
    | [] -> found
    | (i, x) :: pairs when x = value -> get_indices (i :: found) pairs
    | _ :: xs -> get_indices found xs
  in
  get_indices [] (row |> Array.mapi (fun i v -> (i, v)) |> Array.to_list)

let find (value : 'a) (m : 'a t) : (int * int) option =
  m.items
  |> Array.mapi (fun i row -> (i, get_index value row))
  |> Array.find_map (fun (i, row) -> Option.map (fun j -> (i, j)) row)

let find_all value m : (int * int) Seq.t =
  m.items
  |> Array.mapi (fun i row -> get_index_all value row |> List.map (fun j -> (i, j)))
  |> Array.to_list
  |> List.flatten
  |> List.to_seq

let from_string parse lines : 'a t =
  String.split_on_char '\n' lines
  |> List.map (List.map parse << String.split_on_char ' ')
  |> of_list
