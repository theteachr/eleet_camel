open Stdplus

type input = int Matrix.t
type output = int list

let to_string xs = List.map Int.to_string xs |> String.concat " "
let parse lines = Matrix.from_string int_of_string lines

module Direction = struct
  type t =
    | Up
    | Right
    | Down
    | Left

  let next = function
    | Up -> Right
    | Right -> Down
    | Down -> Left
    | Left -> Up
end

module Point = struct
  type t = int * int

  open Direction

  let move delta direction (row, col) =
    match direction with
    | Up -> (row - delta, col)
    | Right -> (row, col + delta)
    | Down -> (row + delta, col)
    | Left -> (row, col - delta)

  let next = move 1
  let prev = move (-1)
end

let solve matrix =
  let matrix = Matrix.map Cell.unvisited matrix in
  let rec spiral visited moving loc =
    let value_opt, new_direction, new_loc =
      match Matrix.at loc matrix with
      | Some (Unvisited _ as x) -> (Some x, moving, loc)
      | _ ->
          let new_direction = Direction.next moving in
          let new_loc = Point.next new_direction (Point.prev moving loc) in
          (Matrix.at new_loc matrix, new_direction, new_loc)
    in
    match value_opt with
    | Some (Unvisited value) ->
        Matrix.update new_loc Cell.Visited matrix;
        let new_loc = Point.next new_direction new_loc in
        spiral (value :: visited) new_direction new_loc
    | _ -> List.rev visited
  in
  spiral [] Direction.Right (0, 0)
