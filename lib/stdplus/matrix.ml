type 'a t =
  { items : 'a array array
  ; dimensions : int * int
  }

let of_list l =
  let items = Array.of_list l |> Array.map Array.of_list in
  let num_rows = List.length l in
  let num_cols = List.(length (hd l)) in
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
