let strings =
  [ "zero"
  ; "one"
  ; "two"
  ; "three"
  ; "four"
  ; "five"
  ; "six"
  ; "seven"
  ; "eight"
  ; "nine"
  ]

let rev_string s =
  s
  |> String.to_seq
  |> List.of_seq
  |> List.rev_map (Printf.sprintf "%c")
  |> String.concat ""

let gnirtss = List.map rev_string strings

let digits = Prefix_tree.new_node ()

let tigids = Prefix_tree.new_node ()

let () =
  let insert_into_pt trie i word =
    Printf.printf "Inserting %s at %i\n" word i;
    Prefix_tree.insert word trie i
  in
  List.iteri (insert_into_pt digits) strings;
  List.iteri (insert_into_pt tigids) gnirtss

let test = "zoneight234"

let digit_of_char c =
  let code = Char.code c in
  if code >= Char.code '0' && code <= Char.code '9' then Some (code - 48)
  else None

let tests =
  [ "two1nine"
  ; "eightwothree"
  ; "abcone2threexyz"
  ; "xtwone3four"
  ; "4nineeightseven2zoneiht234"
  ; "7pqrstsixteen"
  ]

let solve test =
  let scan (chars : char Seq.t) trie =
    let rec scan_until chars trie =
      match Seq.uncons chars with
      | Some (c, rest) -> (
          match Prefix_tree.advance_by_char c trie with
          | Some { value = Some digit; _ } -> Some digit
          | Some ({ value = None; _ } as node) -> scan_until rest node
          | None -> None)
      | None -> None
    in
    let rec scan' chars =
      match Seq.uncons chars with
      | Some (c, rest) -> (
          Printf.printf "%c\n" c;
          match digit_of_char c with
          | Some x ->
              Printf.printf "%d" x;
              Some x
          | None -> (
              match scan_until chars trie with
              | Some x -> Some x
              | None -> scan' rest))
      | None -> None
    in
    scan' chars
  in
  ( scan (String.to_seq test) digits
  , scan (rev_string test |> String.to_seq) tigids )
