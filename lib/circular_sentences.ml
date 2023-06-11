type input = string

type output = bool

let to_string = Bool.to_string

let parse = Fun.id

let last_char s = String.length s - 1 |> String.get s

let first_char s = String.get s 0

let solve test =
  let rec is_circular = function
    | [] | [ _ ] -> true
    | x :: x' :: rest when last_char x = first_char x' -> is_circular rest
    | _ -> false
  in
  String.split_on_char ' ' test |> is_circular
