open Stdplus.Infix

module Bracket = struct
  type shape =
    | Round
    | Curly
    | Square

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

type input = Bracket.t Seq.t

type output = bool

let parse line =
  line
  |> String.to_seq
  |> Seq.map (Option.get << Bracket.of_char)

let to_string = Bool.to_string

let rec balanced open_brackets brackets =
  let open Bracket in
  match (open_brackets, Seq.uncons brackets) with
  | [] , None -> true
  | _, Some (Open b, bs) -> balanced (b :: open_brackets) bs
  | b' :: bs', Some (Close b, bs) when b = b' -> balanced bs' bs
  | _ -> false

let solve = balanced []
