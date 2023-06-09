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

let pairs : (string * (module Solution)) list = [ ("697", (module Degree)) ]
let solutions = pairs |> string_map_of_pairs

let tests_file = Printf.sprintf "tests/%s/tests.txt"

let load_tests problem_id =
  In_channel.(with_open_text (tests_file problem_id) input_all)
  |> Str.(split @@ regexp "---\n")
  |> List.map String.trim

let run_solutions tests (module Solver : Solution) =
  let get_solution_string test =
    let sol_str = test
    |> Solver.parse
    |> Solver.solve
    |> Solver.to_string
    in
    sol_str ^ "\n"
  in
  tests
  |> List.map get_solution_string
  |> String.concat "---\n"

let () =
  let output_file_solution (problem_id, solver) =
    let tests = load_tests problem_id in
    let solutions = run_solutions tests solver in
    let solutions_file =
      let tests_dir = Filename.dirname (tests_file problem_id) in
      Filename.concat tests_dir "expected.txt"
    in
    (solutions_file, solutions)
  in
  let write_solution (file, answer) =
    let open Out_channel in
    let out_channel = open_text file in
    output_string out_channel answer
  in
  solutions
  |> Solutions.bindings
  |> List.map output_file_solution
  |> List.iter write_solution
