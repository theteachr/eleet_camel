open Collections
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
    | "+" -> Ok (Op Op.Add)
    | "-" -> Ok (Op Op.Sub)
    | "*" -> Ok (Op Op.Mul)
    | "/" -> Ok (Op Op.Div)
    | s -> begin
        int_of_string_opt s
        |> Option.map num
        |> Option.to_result ~none:`Invalid_token
      end
end

let parse line =
  String.split_on_char ' ' line |> List.map (Result.get_ok << Token.of_string)

type input = Token.t list

type output = int option

let to_string = function
  | Some answer -> Int.to_string answer
  | None -> "Err..."

let eval_once op out =
  let open Deq in
  let* rite, out = pop_back out in
  let* left, out = pop_back out in
  let res = (Op.f op) left rite in
  Some (res >>: out)

let rec eval_rpn tokens out =
  let open Deq in
  match tokens with
  | [] when length out = 1 -> Some (pop_front out |> Option.get)
  | Token.Num n :: ts -> eval_rpn ts (n >>: out)
  | Token.Op op :: ts -> eval_once op out >>= eval_rpn ts
  | _ -> None

let solve tokens = eval_rpn tokens Deq.empty |> Option.map fst
