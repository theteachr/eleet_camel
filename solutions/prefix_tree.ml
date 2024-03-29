open Stdplus
open Option.Infix

let char_index ch = Char.code ch - Char.code 'a'

type trie_node =
  { mutable end_of_word : bool
  ; paths : trie_node option array
  }

let new_node () = { end_of_word = false; paths = Array.make 26 None }

let insert s root =
  let set_new_node index paths =
    let node = new_node () in
    paths.(index) <- Some node;
    node
  in
  let next_node ch paths =
    let index = char_index ch in
    match paths.(index) with
    | Some node -> node
    | None -> set_new_node index paths
  in
  let rec ins root chars =
    match Seq.uncons chars with
    | None -> root.end_of_word <- true
    | Some (ch, chars') -> ins (next_node ch root.paths) chars'
  in
  ins root (String.to_seq s)

let rec advance chars node =
  match Seq.uncons chars with
  | None -> Some node
  | Some (ch, chars') -> node.paths.(char_index ch) >>= advance chars'

let search text root =
  advance (String.to_seq text) root
  |> Option.fold ~none:false ~some:(fun node -> node.end_of_word)

let starts_with prefix root =
  advance (String.to_seq prefix) root |> Option.is_some

module Command = struct
  type t =
    | New
    | Insert of string
    | Search of string
    | Starts_with of string

  type out =
    | Empty of trie_node
    | Unit
    | Boolean of bool

  let of_string_exn line =
    match String.split_on_char ':' line with
    | [ "Trie" ] -> New
    | [ "insert"; s ] -> Insert s
    | [ "search"; s ] -> Search s
    | [ "startsWith"; s ] -> Starts_with s
    | _ -> failwith "Unknown command"

  let execute cmd trie =
    match cmd with
    | New -> Empty (new_node ())
    | Insert s ->
        insert s trie;
        Unit
    | Search s -> Boolean (search s trie)
    | Starts_with s -> Boolean (starts_with s trie)
end

type input = Command.t list

type output = Command.out list

let string_of_cmd_out = function
  | Command.Unit | Command.Empty _ -> "()"
  | Command.Boolean b -> Bool.to_string b

let parse lines =
  lines |> String.split_on_char '\n' |> List.map Command.of_string_exn

let to_string cmd_outs =
  cmd_outs |> List.map string_of_cmd_out |> String.concat " "

let solve commands =
  let run (trie, outputs) cmd =
    let out = Command.execute cmd trie in
    let updated_trie =
      match out with
      | Empty trie -> trie
      | Unit | Boolean _ -> trie
    in
    (updated_trie, out :: outputs)
  in
  commands |> List.fold_left run (new_node (), []) |> snd |> List.rev
