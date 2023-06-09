open Stdplus.Infix

module Op = struct
  type t =
    | Add
    | Sub
    | Mul
    | Div

  let f = function
    | Add -> ( + )
    | Sub -> ( - )
    | Mul -> ( * )
    | Div -> ( / )

  let to_char = function
    | Add -> '+'
    | Sub -> '-'
    | Mul -> '*'
    | Div -> '/'
end

module Token = struct
  type t =
    | Num of int
    | Op of Op.t

  let num n = Num n

  let of_string = function
    | "+" -> Some (Op Op.Add)
    | "-" -> Some (Op Op.Sub)
    | "*" -> Some (Op Op.Mul)
    | "/" -> Some (Op Op.Div)
    | s -> int_of_string_opt s |> Option.map num
end

let parse line =
  String.split_on_char ' ' line |> List.map (Option.get << Token.of_string)

type input = Token.t list

type output = int option

let to_string = function
  | Some answer -> Int.to_string answer
  | None -> "Err..."

let rec eval_rpn out tokens =
  match (tokens, out) with
  | [], [ x ] -> Some x
  | Token.Num n :: ts, out -> eval_rpn (n :: out) ts
  | Token.Op op :: ts, r :: l :: out -> eval_rpn ((Op.f op) l r :: out) ts
  | _ -> None

let solve = eval_rpn []
