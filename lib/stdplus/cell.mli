type 'a t =
  | Visited
  | Unvisited of 'a

val unvisited : 'a -> 'a t
