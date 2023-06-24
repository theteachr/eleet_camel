open Stdplus.Infix
open Stdplus.Lists

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

let solve (l_one, l_two) =
  let add_with_carry (c, sums) (a, b) =
    let sum = a + b + c in
    let c, units_digit = sum /% 10 in
    (c, units_digit :: sums)
  in
  let zipped = zip_longest l_one l_two ~default:0 in
  let carry, sums = List.fold_left add_with_carry (0, []) zipped in
  let sums = if carry = 1 then 1 :: sums else sums in
  List.rev sums
