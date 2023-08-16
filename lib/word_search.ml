open Stdplus

type 'a cell = Visited of 'a | Unvisited of 'a

type input = char list * char cell Matrix.t

type output = bool

let char_of_string s = String.get s 0

let string_of_char_list chars =
  chars |> List.map (String.make 1) |> String.concat ""

let parse text =
  let word, board =
    match String.split_on_char '\n' text with
    | word :: board -> (word, List.map (String.split_on_char ' ') board)
    | [] -> failwith "Bad input"
  in
  ( word |> String.to_seq |> List.of_seq
  , board |> Matrix.of_list |> Matrix.map (fun s -> Unvisited (char_of_string s)) )

let to_string = Bool.to_string

let is_valid (row_max, col_max) (row, col) =
  [ row >= 0; row < row_max; col >= 0; col < col_max ] |> List.for_all Fun.id

let print_point_line (row, col) =
  Printf.printf "(%d, %d)\n" row col

let valid_neighbors board (row, col) =
  [ (row, col + 1); (row + 1, col); (row, col - 1); (row - 1, col) ]
  |> List.filter (is_valid board)

let neighbors (row, col) =
  [ (row, col + 1); (row + 1, col); (row, col - 1); (row - 1, col) ]

module Point = struct
  type t = int * int

  let compare (x1, y1) (x2, y2) =
    if x1 = x2 && y1 = y2 then 0 else (x1 - x2) * (y1 - y2)

  let to_string (x, y) = Printf.sprintf "[%d, %d]" x y
end

module PointSet = Set.Make (Point)

(*

[ c ]

[ [ b c ]
; [ a e]
]

search [ c ] board 0.0

*)

let solve (word, board) =
  let rec search (chars : char list) (board : char cell Matrix.t)
      (point : Point.t) =
    Printf.printf "Current point: %s\n" @@ Point.to_string point;
    match (chars, Matrix.at point board) with
    | [], _ -> true
    | _, None ->
        print_endline "The point is out of bounds";
        false
    | _, Some (Visited c) ->
        Printf.printf "'%c' is already visited\n" c;
        Matrix.update point (Unvisited c) board;
        false
    | input_char :: chars, Some (Unvisited board_char)
      when input_char = board_char ->
        Printf.printf "Input char: %c, Board char: %c\n" input_char board_char;
        Matrix.update point (Visited input_char) board;
        neighbors point |> List.map Point.to_string |> String.concat " "
        |> print_endline;
        neighbors point |> List.map (search chars board) |> List.exists Fun.id
    | _, Some (Unvisited c) ->
        Printf.printf "Non matching character: '%c'\n" c;
        false
  in
  match word with
  | [] -> false
  | letter :: _ -> (
      match Matrix.find (Unvisited letter) board with
      | Some point -> search word board point
      | None -> false)
