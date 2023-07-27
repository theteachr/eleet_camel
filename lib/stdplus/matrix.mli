type 'a t

val of_list : 'a list list -> 'a t

val at : int * int -> 'a t -> 'a option

val at_exn : int * int -> 'a t -> 'a

val dimensions : 'a t -> int * int
