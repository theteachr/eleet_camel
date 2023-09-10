module Bracket = struct
  type shape =
    | Round
    | Curly
    | Square

  type t =
    | Open of shape
    | Close of shape

  let of_char_exn = function
    | '(' -> Open Round
    | '{' -> Open Curly
    | '[' -> Open Square
    | ')' -> Close Round
    | '}' -> Close Curly
    | ']' -> Close Square
    | _ -> failwith "Not a bracket"

  let of_char c = try Some (of_char_exn c) with _ -> None
end

type input = Bracket.t Seq.t

type output = bool

let parse line = line |> String.to_seq |> Seq.map Bracket.of_char_exn

let to_string = Bool.to_string

let rec balanced open_brackets brackets =
  let open Bracket in
  match (open_brackets, Seq.uncons brackets) with
  | [], None -> true
  | _, Some (Open open_bracket, brackets) ->
      balanced (open_bracket :: open_brackets) brackets
  | open_bracket :: rest, Some (Close closed_bracket, brackets)
    when open_bracket = closed_bracket -> balanced rest brackets
  | _ -> false

let solve = balanced []
