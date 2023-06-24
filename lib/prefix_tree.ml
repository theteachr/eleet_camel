open Stdplus.Infix

module Command = struct
  type t =
    | New
    | Insert of string
    | Search of string
    | Starts_with of string

  let of_string line =
    match String.split_on_char ':' line with
    | [ "Trie" ] -> Some New
    | [ "insert"; s ] -> Some (Insert s)
    | [ "search"; s ] -> Some (Search s)
    | [ "startsWith"; s ] -> Some (Starts_with s)
    | _ -> None
end

type input = Command.t list

type cmd_out =
  | Unit
  | Boolean of bool

type output = cmd_out list 

let parse lines =
  String.split_on_char '\n' lines
  |> List.map (Option.get << Command.of_string)

let string_of_cmd_out = function
  | Unit -> "()"
  | Boolean b -> Bool.to_string b

let to_string cmd_outs =
  cmd_outs
  |> List.map string_of_cmd_out
  |> String.concat " "

let solve _ = [Unit; Unit; Boolean false]
