type input = int list

type output = int

let parse line = line |> String.split_on_char ' ' |> List.map int_of_string

let to_string = Int.to_string

let rec rob_max sum_pp sum_p = function
  | [] -> sum_p
  | [ h ] -> sum_pp + h
  | [ h1; h2 ] -> Int.max (h1 + sum_pp) (h2 + sum_p) |> Int.max (h2 + sum_pp)
  | h1 :: h2 :: houses -> rob_max (h1 + sum_pp) (h2 + sum_p) houses

let solve = rob_max 0 0
