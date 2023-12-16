open Stdplus
open Option.Infix

let char_index ch = Char.code ch - Char.code 'a'

type 'a trie_node =
  { mutable value : 'a option
  ; paths : 'a trie_node option array
  }

let new_node () = { value = None; paths = Array.make 26 None }

let insert s root value =
  let set_new_node index paths =
    let node = new_node () in
    paths.(index) <- Some node;
    node
  in
  let next_node ch paths =
    let index = char_index ch in
    Option.value paths.(index) ~default:(set_new_node index paths)
  in
  let rec ins root chars =
    match Seq.uncons chars with
    | None -> root.value <- Some value
    | Some (ch, chars') -> ins (next_node ch root.paths) chars'
  in
  ins root (String.to_seq s)

let advance text node =
  let rec move chars node =
    match Seq.uncons chars with
    | None -> Some node
    | Some (ch, chars') -> node.paths.(char_index ch) >>= move chars'
  in
  move (String.to_seq text) node

let advance_by_char c node = node.paths.(char_index c)

let search text root =
  advance text root
  |> Option.fold ~none:false ~some:(fun node -> Option.is_some node.value)

let starts_with prefix root = advance prefix root |> Option.is_some

module Command = struct
  type t =
    | New
    | Insert of string
    | Search of string
    | Starts_with of string

  type out =
    | Empty of int trie_node
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
        insert s trie 0;
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
