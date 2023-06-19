let ( << ) f g x = x |> g |> f

(* FIXME: Not an infix op: Move this to a better location. *)
let ( let* ) opt f = Option.bind opt f

let ( >>= ) = Option.bind
