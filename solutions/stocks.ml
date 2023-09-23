type input = int list

type output = int

let parse line = line |> String.split_on_char ' ' |> List.map int_of_string

let to_string = Int.to_string

let solve prices =
  let max_profit (curr_min, curr_max_profit) price =
    (Int.min curr_min price, Int.max (price - curr_min) curr_max_profit)
  in
  List.fold_left max_profit (List.hd prices, 0) (List.tl prices) |> snd
