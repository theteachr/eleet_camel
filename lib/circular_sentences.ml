type input = string

type output = bool

let to_string = Bool.to_string

let parse = Fun.id

let last_char s = String.length s - 1 |> String.get s

let first_char s = String.get s 0

let solve test =
  let words = String.split_on_char ' ' test in
  (* Just keep the _fi_rst and _la_st character_s_ of every word. *)
  let filas = List.map (fun w -> (first_char w, last_char w)) words in
  (* Get the first character of the first word to compare it later
     with the last character of the last word. *)
  let first, _ = List.hd filas in
  let rec is_circular = function
    | [] -> true
    | [ (_, c) ] -> c = first
    | (_, x) :: ((x', _) :: _ as rest) when x = x' -> is_circular rest
    | _ -> false
  in
  is_circular filas
