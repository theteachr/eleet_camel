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

let char_index c = Char.code c - Char.code 'a'

module Prefix_tree = struct
  type t =
    { ends : bool
    ; paths : t option array
    }

  let empty_paths = Array.make 26 None

  let empty = { ends = false; paths = empty_paths }

  let insert s tree =
    let rec ins chars tree =
      match (tree, chars) with
      | ({ paths; _ } as node), c :: [] ->
          let idx = char_index c in
          let path_node =
            try Array.get paths idx with Invalid_argument _ -> Some empty
          in
          Array.set paths idx { path_node with ends = true };
          node
      | ({ paths; _ } as node), c :: cs ->
          let new_node = ins cs empty in
          Array.set paths (char_index c) (Some new_node);
          node
      | node, [] -> node
    in
    ins (s |> String.to_seq |> List.of_seq) tree

  let search _ _ = false

  let starts_with _ _ = false
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
