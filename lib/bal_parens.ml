type bracket = Round | Curly | Square
type opening_bracket = Opening of bracket

module Bracket_state = struct
  type t = Open of bracket | Close of bracket

  let of_char = function
    | '(' -> Some (Open Round)
    | '{' -> Some (Open Curly)
    | '[' -> Some (Open Square)
    | ')' -> Some (Close Round)
    | '}' -> Some (Close Curly)
    | ']' -> Some (Close Square)
    | _ -> None
end

type input = Bracket_state.t list
type output = bool

let parse line =
  line
  |> String.to_seq
  |> List.of_seq
  |> List.filter_map Bracket_state.of_char

let to_string = Bool.to_string

let solve brackets =
  let open Bracket_state in
  let rec aux input st = match (input, st) with
    | [], [] -> true
    | Open b :: bs, _ -> aux bs ((Opening b) :: st)
    | Close b :: bs, Opening b' :: bs' when b = b' -> aux bs bs'
    | _ -> false
  in
  aux brackets []
