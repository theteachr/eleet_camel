include module type of Stdlib.List

val enumerate : 'a list -> (int * 'a) list

val zip_longest : 'a list -> 'a list -> default:'a -> ('a * 'a) list

val any : bool list -> bool
