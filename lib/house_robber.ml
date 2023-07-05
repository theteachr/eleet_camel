type input = int list

type output = int

let parse line = line |> String.split_on_char ' ' |> List.map int_of_string

let to_string = Int.to_string

let rec rob_max sum = function
  | [] -> sum
  | [ h ] -> sum + h
  | [ h1; h2 ] -> sum + Int.max h1 h2
  | [ h1; h2; h3 ] -> sum + Int.max (h1 + h3) h2
  | h1 :: h2 :: h3 :: (_ :: houses as rest) ->
      let sum_one = rob_max (sum + h1 + h3) houses in
      let sum_two = rob_max (sum + h2) rest in
      let sum_three = rob_max (sum + h1) rest in
      Int.max sum_one sum_two |> Int.max sum_three

let solve = rob_max 0
