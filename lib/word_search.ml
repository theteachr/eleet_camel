open Stdplus

type input = char list * char Matrix.t

type output = bool

let char_of_string s = String.get s 0

let parse text =
  let word, board =
    match String.split_on_char '\n' text with
    | word :: board -> (word, List.map (String.split_on_char ' ') board)
    | [] -> failwith "Bad input"
  in
  ( word |> String.to_seq |> List.of_seq
  , board |> Matrix.of_list |> Matrix.map char_of_string )

let to_string = Bool.to_string

let neighbors (row, col) =
  [ (row, col + 1); (row + 1, col); (row, col - 1); (row - 1, col) ]

module Point = struct
  type t = int * int

  let compare (x1, y1) (x2, y2) = (x1 - x2) + (y1 - y2)
end

module PointSet = Set.Make(Point)

(* A :: BCCED *)
(* w :: ws *)
let rec search (word : char list) (board : char Matrix.t) (visited : PointSet.t) (curr_point : Point.t) =
  match (word, Matrix.at curr_point board) with
  | [], _ -> true
  | _ when PointSet.exists (( = ) curr_point) visited -> false
  | w :: ws, Some c ->
      let remainder = if w = c then ws else word in
      let visited = PointSet.add curr_point visited in
      neighbors curr_point |> List.map (search remainder board visited) |> List.exists Fun.id
  | _ -> false

let solve (word, board) = search word board PointSet.empty (0, 0)
