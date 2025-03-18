open Stdplus.Infix
open Stdplus

type input = int list * int list

type output = int list

let parse lines =
  let int_lines =
    lines
    |> String.split_on_char '\n'
    |> List.map (List.map int_of_string << String.split_on_char ' ')
  in
  match int_lines with
  | [ first; second ] -> (first, second)
  | _ -> failwith "Bad input"

let to_string output = output |> List.map Int.to_string |> String.concat " "

let solve ns =
  let rec add carry = function
    | [], xs | xs, [] -> if carry = 0 then xs else add 0 (xs, [carry])
    | x :: xs, y :: ys ->
        let sum = x + y + carry in
        (sum mod 10) :: add (sum / 10) (xs, ys)
  in
  add 0 ns
