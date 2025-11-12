type input = int list
type output = int

let parse line = line |> String.split_on_char ' ' |> List.map int_of_string
let to_string = Int.to_string

let rec rob prev curr = function
  | [] -> Int.max prev curr
  | h :: houses -> rob curr (Int.max curr (h + prev)) houses

let solve = rob 0 0
