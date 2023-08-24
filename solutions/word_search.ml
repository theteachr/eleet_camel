open Stdplus
open Stdplus.Infix

type input = char list * char Cell.t Matrix.t

type output = bool

let char_of_string s = String.get s 0

let string_of_char_list chars =
  chars |> List.map (String.make 1) |> String.concat ""

let parse text =
  let chars, board =
    match String.split_on_char '\n' text with
    | word :: board -> (word, List.map (String.split_on_char ' ') board)
    | [] -> failwith "Bad input"
  in
  ( chars |> String.to_seq |> List.of_seq
  , board |> Matrix.of_list |> Matrix.map (Cell.unvisited << char_of_string) )

let to_string = Bool.to_string

let neighbors (row, col) =
  [ (row, col + 1); (row + 1, col); (row, col - 1); (row - 1, col) ]
  |> List.to_seq

let solve (word, board) =
  let rec search chars loc =
    match (chars, Matrix.at loc board) with
    | [], _ -> true
    | input_char :: chars, Some (Cell.Unvisited board_char)
      when input_char = board_char ->
        Matrix.update loc Cell.Visited board;
        let found = neighbors loc |> Seq.map (search chars) |> Seq.any in
        Matrix.update loc (Cell.Unvisited input_char) board;
        found
    | _ -> false
  in
  match word with
  | letter :: _ ->
      Matrix.find_all (Cell.Unvisited letter) board
      |> Seq.map (search word)
      |> Seq.any
  | [] -> false
