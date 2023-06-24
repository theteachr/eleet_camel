open Stdplus.Infix

type input = int list * int list

type output = int list

let parse lines =
  let int_lines = lines
    |> String.split_on_char '\n'
    |> List.map (List.map int_of_string << String.split_on_char ' ')
  in
  match int_lines with
  | [ first; second ] -> (first, second)
  | _ -> failwith "Bad input"

let to_string output =
  output
  |> List.map Int.to_string
  |> String.concat " "

let rec add_two_lists carry sums = function
  | x :: xs, y :: ys ->
      let sum = x + y + carry in
      let carry, value_at_ones = sum /% 10 in
      add_two_lists carry (value_at_ones :: sums) (xs, ys)
  | [], zs | zs, [] ->
      if carry = 0 then List.rev_append sums zs
      else add_two_lists 1 sums (zs, [ 0 ])

let solve = add_two_lists 0 []
