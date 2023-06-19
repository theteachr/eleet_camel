type 'a t

val empty : 'a t

val push_back : 'a -> 'a t -> 'a t

val push_front : 'a -> 'a t -> 'a t

val pop_back : 'a t -> ('a * 'a t) option

val pop_front : 'a t -> ('a * 'a t) option

val length : 'a t -> int

val ( >>: ) : 'a -> 'a t -> 'a t
