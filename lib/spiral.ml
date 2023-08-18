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

  let to_string = function
    | Up -> "up"
    | Right -> "right"
    | Down -> "down"
    | Left -> "left"
end

module Point = struct
  type t = int * int

  let next direction (row, col) =
    let open Direction in 
    match direction with
    | Up -> (row - 1, col)
    | Right -> (row, col + 1)
    | Down -> (row + 1, col)
    | Left -> (row, col - 1)

  let prev direction (row, col) =
    let open Direction in 
    match direction with
    | Up -> (row + 1, col)
    | Right -> (row, col - 1)
    | Down -> (row - 1, col)
    | Left -> (row, col + 1)
end

module Cell = struct
  type 'a t =
    | Visited
    | Unvisited of 'a

  let unvisited x = Unvisited x
end

let solve (matrix : input) =
  let matrix = Matrix.map Cell.unvisited matrix in
  let rec spiral visited moving ((row, col) as loc) =
    let open Printf in
    printf "At: %d, %d\n" row col;
    printf "Moving %s\n" @@ Direction.to_string moving;
    let value_opt, new_direction, new_loc =
      match Matrix.at loc matrix with
      | Some (Unvisited _ as x) -> (Some x, moving, loc)
      | _ ->
          let new_direction = Direction.next moving in
          printf "Changing direction to: %s\n" @@ Direction.to_string new_direction;
          let ((row, col) as new_loc) = Point.next new_direction (Point.prev moving loc) in
          printf "New location: %d, %d\n" row col;
          (Matrix.at new_loc matrix, new_direction, new_loc)
    in
    match value_opt with
    | Some (Unvisited value) ->
        printf "Value found: %d\n" value;
        Matrix.update new_loc Cell.Visited matrix;
        let new_loc = Point.next new_direction new_loc in
        spiral (value :: visited) new_direction new_loc
    | _ -> List.rev visited
  in
  spiral [] Direction.Right (0, 0)
