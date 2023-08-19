type 'a t =
  | Visited
  | Unvisited of 'a

let unvisited x = Unvisited x
