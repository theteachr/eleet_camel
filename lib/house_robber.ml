type input = int list

type output = int

let parse line = line |> String.split_on_char ' ' |> List.map int_of_string

let to_string = Int.to_string

let rec rob pp p = function
  | [] -> p
  | [ h ] -> pp + h
  | [ h1; h2 ] -> Int.max (h1 + pp) (h2 + p) |> Int.max (h2 + pp)
  | h1 :: h2 :: houses -> rob (h1 + pp) (h2 + p) houses

let solve = rob 0 0
