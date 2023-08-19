(* FIXME: The same piece is repated in the interface file. *)
type 'a t =
  | Visited
  | Unvisited of 'a

let unvisited x = Unvisited x
