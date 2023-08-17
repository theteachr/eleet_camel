include Stdlib.List

let enumerate lst = Stdlib.List.mapi (fun i x -> (i, x)) lst

let zip_longest l_one l_two ~default =
  let rec zip pairs = function
    | [], [] -> Stdlib.List.rev pairs
    | x :: xs, y :: ys -> zip ((x, y) :: pairs) (xs, ys)
    | x :: xs, [] -> zip ((x, default) :: pairs) (xs, [])
    | [], y :: ys -> zip ((default, y) :: pairs) ([], ys)
  in
  zip [] (l_one, l_two)
