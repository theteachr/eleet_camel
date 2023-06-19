(* TODO: Use a better structure for O(1) pushing and popping *)
type 'a t = 'a list

let empty = []

let push_front = List.cons

let push_back x xs = xs @ [x]

let pop_front = function [] -> None | x :: xs -> Some (x, xs)

let pop_back q = 
  let rec pop elems = function
  | [] -> None
  | x :: [] -> Some (x, List.rev elems)
  | x :: xs -> pop (x :: elems) xs
  in
  pop [] q

let ( >>: ) = push_back

let length = List.length
