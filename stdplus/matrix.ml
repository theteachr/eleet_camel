open Infix

type 'a t = 'a array array

let of_list l = Array.of_list l |> Array.map Array.of_list

let at (row, col) m = try Some m.(row).(col) with Invalid_argument _ -> None

let at_exn (row, col) m = m.(row).(col)

let dimensions m = Array.(length m, length m.(0))

let print_row (row : string array) =
  String.concat " " (Array.to_list row) |> print_endline

let map f m = Array.map (Array.map f) m

let print m = Array.iter print_row m

let update (row, col) value m = m.(row).(col) <- value

let get_index value row =
  row
  |> Array.mapi (fun i v -> (i, v))
  |> Array.find_map (fun (i, v) -> if value = v then Some i else None)

let get_index_all value row : int list =
  row
  |> Array.to_list
  |> List.enumerate
  |> List.filter_map (fun (i, x) -> if value = x then Some i else None)

let find value m =
  m
  |> Array.mapi (fun i row -> (i, get_index value row))
  |> Array.find_map (fun (i, row) -> Option.map (fun j -> (i, j)) row)

let find_all value m =
  let pair_with_row_index (i, row) =
    get_index_all value row |> List.rev_map (fun j -> (i, j))
  in
  m
  |> Array.to_list
  |> List.enumerate
  |> List.rev_map pair_with_row_index
  |> List.concat

let from_string parse lines =
  lines
  |> String.split_on_char '\n'
  |> List.map (List.map parse << String.split_on_char ' ')
  |> of_list

let mark (row, col) m = m.(row).(col) <- Cell.Visited

let unmark (row, col) x m = m.(row).(col) <- Cell.Unvisited x
