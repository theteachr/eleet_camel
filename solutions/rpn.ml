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

  let of_string_exn = function
    | "+" -> Op Op.Add
    | "-" -> Op Op.Sub
    | "*" -> Op Op.Mul
    | "/" -> Op Op.Div
    | s -> Num (int_of_string s)

  let of_string s = try Some (of_string_exn s) with _ -> None
end

let parse line = String.split_on_char ' ' line |> List.map Token.of_string_exn

type input = Token.t list

type output = int option

let to_string = function
  | Some answer -> Int.to_string answer
  | None -> "Err..."

let rec eval_rpn out tokens =
  match (out, tokens) with
  | [ x ], [] -> Some x
  | out, Token.Num n :: ts -> eval_rpn (n :: out) ts
  | r :: l :: out, Token.Op op :: ts -> eval_rpn ((Op.f op) l r :: out) ts
  | _ -> None

let solve = eval_rpn []
