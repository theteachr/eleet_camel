open Stdplus.Infix

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
  let rec add carry res = function
    | [], xs | xs, [] ->
        if carry = 0 then List.rev_append res xs else add 0 res (xs, [ carry ])
    | x :: xs, y :: ys ->
        let sum = x + y + carry in
        add (sum / 10) ((sum mod 10) :: res) (xs, ys)
  in
  add 0 [] ns
