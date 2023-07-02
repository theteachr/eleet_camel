open Stdplus.Infix

let char_index ch = Char.code ch - Char.code 'a'

type trie_node =
  { mutable ends : bool
  ; paths : trie_node option array
  }

let new_paths () = Array.make 26 None

let new_node () = { ends = false; paths = new_paths () }

let chars_of_string s = s |> String.to_seq |> List.of_seq

let insert s root =
  let next_node ch { paths; _ } =
    let index = char_index ch in
    match paths.(index) with
    | Some node -> node
    | None ->
        let node = new_node () in
        paths.(index) <- Some node;
        node
  in
  let rec ins root = function
    | [] -> root.ends <- true
    | ch :: chs -> ins (next_node ch root) chs
  in
  chars_of_string s |> ins root

let search text root =
  let rec exists chs node =
    match (chs, node) with
    | [], { ends; _ } -> ends
    | ch :: chs, { paths; _ } ->
        paths.(char_index ch)
        |> Option.map (exists chs)
        |> Option.value ~default:false
  in
  exists (chars_of_string text) root

let starts_with prefix root =
  let rec is_prefix chs node =
    match (chs, node) with
    | [], _ -> true
    | ch :: chs, { paths; _ } ->
        paths.(char_index ch)
        |> Option.map (is_prefix chs)
        |> Option.value ~default:false
  in
  is_prefix (chars_of_string prefix) root

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

  let of_string line =
    match String.split_on_char ':' line with
    | [ "Trie" ] -> Some New
    | [ "insert"; s ] -> Some (Insert s)
    | [ "search"; s ] -> Some (Search s)
    | [ "startsWith"; s ] -> Some (Starts_with s)
    | _ -> None

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
  String.split_on_char '\n' lines |> List.map (Option.get << Command.of_string)

let to_string cmd_outs =
  cmd_outs
  |> List.map string_of_cmd_out
  |> String.concat " "

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
  List.fold_left run (new_node (), []) commands |> snd |> List.rev
