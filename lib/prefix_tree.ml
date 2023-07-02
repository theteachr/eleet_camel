open Stdplus.Infix

let char_index ch = Char.code ch - Char.code 'a'

type trie_node =
  { mutable end_of_word : bool
  ; paths : trie_node option array
  }

let new_paths () = Array.make 26 None

let new_node () = { end_of_word = false; paths = new_paths () }

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
    | [] -> root.end_of_word <- true
    | ch :: chars -> ins (next_node ch root) chars
  in
  chars_of_string s |> ins root

let rec lookup chars node =
  match (chars, node) with
  | [], { end_of_word; _ } -> `Success end_of_word
  (* XXX: A value like `Success false is extremely confusing. *)
  | ch :: chars, { paths; _ } ->
      paths.(char_index ch)
      |> Option.map (lookup chars)
      |> Option.value ~default:`Failure

let search text root =
  match lookup (chars_of_string text) root with
  | `Success end_of_word -> end_of_word
  | `Failure -> false

let starts_with prefix root =
  match lookup (chars_of_string prefix) root with
  | `Success _ -> true
  | `Failure -> false

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
