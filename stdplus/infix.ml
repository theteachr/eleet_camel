let ( << ) f g x = x |> g |> f

let ( >>= ) = Option.bind

let ( /% ) x y = (x / y, x mod y)

let ( -- ) left rite =
  let rec aux n range = if n < left then range else aux (n - 1) (n :: range) in
  aux rite []
