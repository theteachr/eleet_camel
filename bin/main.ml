open Eleet_camel
open Stdplus.Infix

module type Solution = sig
  type input

  type output

  val parse : string -> input

  val solve : input -> output

  val to_string : output -> string
end

module Solutions = Map.Make (String)

let string_map_of_pairs =
  List.fold_left (fun m (n, v) -> Solutions.add n v m) Solutions.empty

let solvers : (string * (module Solution)) list =
  [ ("697", (module Degree))
  ; ("20", (module Bal_parens))
  ; ("1", (module Two_sum))
  ; ("2490", (module Circular_sentences))
  ; ("150", (module Rpn))
  ; ("2", (module Add_two))
  ; ("208", (module Prefix_tree))
  ; ("198", (module House_robber))
  ; ("79", (module Word_search))
  ; ("54", (module Spiral))
  ]

(* This shadowing is done to avoid module type inference complications. *)
let solvers = solvers |> string_map_of_pairs

let test_seperator = "\n---\n\n"

let solution_separator = "===\n"

let tests_file = Printf.sprintf "tests/%s.txt"

let load_tests_from_file f =
  In_channel.(with_open_text f input_all)
  |> Str.(split @@ regexp test_seperator)
  |> List.map (List.hd << Str.(split @@ regexp solution_separator))

let get_solved_entry (module Solver : Solution) test =
  let sol = Solver.(String.trim test |> parse |> solve |> to_string) in
  test ^ solution_separator ^ sol ^ "\n"

let () =
  let out_file_content (problem_id, solver) =
    let out_file = tests_file problem_id in
    let content =
      load_tests_from_file out_file
      |> List.map (get_solved_entry solver)
      |> String.concat test_seperator
    in
    (out_file, content)
  in
  let write_actual (file, new_entry) =
    Out_channel.(output_string (open_text file) new_entry)
  in
  solvers |> Solutions.bindings |> List.map out_file_content
  |> List.iter write_actual
