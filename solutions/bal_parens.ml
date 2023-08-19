open Stdplus.Infix

module Bracket = struct
  type shape =
    | Round
    | Curly
    | Square

  type opening = Opening of shape

  type t =
    | Open of shape
    | Close of shape

  let of_char = function
    | '(' -> Some (Open Round)
    | '{' -> Some (Open Curly)
    | '[' -> Some (Open Square)
    | ')' -> Some (Close Round)
    | '}' -> Some (Close Curly)
    | ']' -> Some (Close Square)
    | _ -> None
end

type input = Bracket.t list

type output = bool

let parse line =
  line
  |> String.to_seq
  |> List.of_seq
  |> List.map (Option.get << Bracket.of_char)

let to_string = Bool.to_string

let rec balanced st brackets =
  let open Bracket in
  match (brackets, st) with
  | [], [] -> true
  | Open b :: bs, _ -> balanced (Opening b :: st) bs
  | Close b :: bs, Opening b' :: bs' when b = b' -> balanced bs' bs
  | _ -> false

let solve = balanced []
