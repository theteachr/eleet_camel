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

let pop_back_n n q =
  let rec pop n popped q =
    match (n, pop_back q) with
    | 0, _ -> Some (popped, q)
    | n, Some (e, q) -> pop (n - 1) (e :: popped) q
    | _ -> None
  in
  pop n [] q

let ( >>: ) = push_back

let length = List.length
