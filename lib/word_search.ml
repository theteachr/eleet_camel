type input = string * string list list

type output = bool

let parse text =
  match String.split_on_char '\n' text with
  | needle :: haystack -> needle, List.map (String.split_on_char ' ') haystack
  | [] -> failwith "Bad input"

let to_string = Bool.to_string

let solve _ = false
