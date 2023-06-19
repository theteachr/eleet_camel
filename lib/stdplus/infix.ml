let ( << ) f g x = x |> g |> f

let ( let* ) opt f = Option.bind opt f

let ( >>= ) = Option.bind
