open Eleet_camel

let () =
  let print_solution = function
    | Some answer -> print_endline (Int.to_string answer)
    | None -> print_endline "¯\\_(ツ)_/¯"
  in
  Degree.solutions |> List.iter print_solution
