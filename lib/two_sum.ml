type input = int list * int
type output = int * int

let parse _ = ([1; 2; 3], 2)
let to_string (first, snd) = Printf.sprintf "%d %d" first snd
let solve _ = (0, 0)
