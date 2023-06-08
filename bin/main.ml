open Eleet_camel

module type Solution = sig
  type input
  type output

  val parse : string -> input
  val solve : input -> output
  val to_string : output -> string
end

let solutions : (module Solution) list = [ (module Degree) ]

let run_solution tests (module Solver : Solution) =
  let print_solution test =
    test |> Solver.parse |> Solver.solve |> Solver.to_string |> print_endline
  in
  tests |> List.iter print_solution

let tests = [ "1 2 2 1 3"; "1 2 2 3 1 4 2" ]
let () = solutions |> List.iter (run_solution tests)
