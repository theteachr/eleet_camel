let ( << ) f g x = x |> g |> f

let ( >>= ) = Option.bind

let ( /% ) x y = (x / y, x mod y)
