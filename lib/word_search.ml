open Stdplus

module Cell = struct
  type 'a t =
    | Visited
    | Unvisited of 'a

  let unvisited x = Unvisited x
end

type input = char list * char Matrix.t

type output = bool

let char_of_string s = String.get s 0

let string_of_char_list chars =
  chars |> List.map (String.make 1) |> String.concat ""

let parse text : input =
  let chars, board =
    match String.split_on_char '\n' text with
    | word :: board -> (word, List.map (String.split_on_char ' ') board)
    | [] -> failwith "Bad input"
  in
  ( chars |> String.to_seq |> List.of_seq
  , board |> Matrix.of_list |> Matrix.map char_of_string )

let to_string = Bool.to_string

let neighbors (row, col) =
  [ (row, col + 1); (row + 1, col); (row, col - 1); (row - 1, col) ]

type point = int * int

let solve (word, board) =
  let rec search (chars : char list) (board : char Cell.t Matrix.t)
      (loc : point) =
    match (chars, Matrix.at loc board) with
    | [], _ -> true
    | _, None -> false
    | _, Some Visited -> false
    | input_char :: chars, Some (Unvisited board_char)
      when input_char = board_char ->
        Matrix.update loc Cell.Visited board;
        neighbors loc |> List.map (search chars board) |> List.any
    | _ -> false
  in
  match word with
  | letter :: _ ->
      Matrix.find_all letter board
      |> List.rev_map (search word @@ Matrix.map Cell.unvisited board)
      |> List.any
  | [] -> false
