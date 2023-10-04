open Stdplus
open Stdplus.Infix

type input = char Seq.t * char Cell.t Matrix.t

type output = bool

let char_of_string s = String.get s 0

let parse text =
  let chars, board =
    match String.split_on_char '\n' text with
    | word :: board -> (word, List.map (String.split_on_char ' ') board)
    | [] -> failwith "Bad input"
  in
  ( chars |> String.to_seq
  , board |> Matrix.of_list |> Matrix.map (Cell.unvisited << char_of_string) )

let to_string = Bool.to_string

let neighbors (row, col) =
  [ (row, col + 1); (row + 1, col); (row, col - 1); (row - 1, col) ]

let solve (word, board) =
  let rec search chars loc =
    match (Seq.uncons chars, Matrix.at loc board) with
    | None, _ -> true
    | Some (input_char, chars), Some (Cell.Unvisited board_char)
      when input_char = board_char ->
        Matrix.mark loc board;
        let found =
          neighbors loc |> List.to_seq |> Seq.map (search chars) |> Seq.any
        in
        Matrix.unmark loc input_char board;
        found
    | _ -> false
  in
  match Seq.uncons word with
  | Some (letter, _) ->
      Matrix.find_all (Cell.Unvisited letter) board
      |> List.to_seq
      |> Seq.map (search word)
      |> Seq.any
  | None -> false
