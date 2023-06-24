open Eleet_camel

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
  ]

(* This shadowing is done to avoid module type inference complications. *)
let solvers = solvers |> string_map_of_pairs

let new_line = "\n"

let test_seperator = "---" ^ new_line

let expected_file = "expected.txt"

let tests_file = Printf.sprintf "tests/%s/tests.txt"

let load_tests problem_id =
  In_channel.(with_open_text (tests_file problem_id) input_all)
  |> Str.(split @@ regexp test_seperator)
  |> List.map String.trim

let run_solutions tests (module Solver : Solution) =
  let get_solution_string test =
    let sol_str =
      test
      |> Solver.parse
      |> Solver.solve
      |> Solver.to_string
    in
    sol_str ^ new_line
  in
  tests
  |> List.map get_solution_string
  |> String.concat test_seperator

let () =
  let output_file_solution (problem_id, solver) =
    let tests = load_tests problem_id in
    let solutions = run_solutions tests solver in
    let solutions_file =
      let tests_dir = Filename.dirname (tests_file problem_id) in
      Filename.concat tests_dir expected_file
    in
    (solutions_file, solutions)
  in
  let write_solution (file, answer) =
    let open Out_channel in
    let out_channel = open_text file in
    output_string out_channel answer
  in
  solvers
  |> Solutions.bindings
  |> List.map output_file_solution
  |> List.iter write_solution
