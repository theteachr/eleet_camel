let enumerate lst = List.mapi (fun i x -> (i, x)) lst

let zip_longest l_one l_two ~default =
  let rec zip acc = function
    | [], [] -> List.rev acc
    | x :: xs, y :: ys -> zip ((x, y) :: acc) (xs, ys)
    | x :: xs, [] -> zip ((x, default) :: acc) (xs, [])
    | [], y :: ys -> zip ((default, y) :: acc) ([], ys)
  in
  zip [] (l_one, l_two)
