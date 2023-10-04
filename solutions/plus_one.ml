type input = int list

type output = int list

let parse line = line |> String.split_on_char ' ' |> List.map int_of_string

let to_string digits = digits |> List.map Int.to_string |> String.concat " "

let append_carry carrying digits = if carrying then 1 :: digits else digits

let rec inc carrying res = function
  | [] -> append_carry carrying res
  | digits when not carrying -> List.rev_append digits (List.rev res)
  | digit :: digits -> inc (digit = 9) (((digit + 1) mod 10) :: res) digits

let solve digits =
  match List.rev digits with
  | [] -> []
  | lsd :: digits when lsd < 9 -> (lsd + 1) :: digits |> List.rev
  | digits -> inc true [] digits
