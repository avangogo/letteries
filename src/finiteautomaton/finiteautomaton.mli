type nondeterministic
type state
type 'a noncomplete
type sigma = int
type t

val pauto : t -> unit
val sigma : t -> int
val size : t -> int

val empty : t

val initState : t -> state
val isFinal : t -> state -> bool
val delta : t -> state -> sigma -> state

val trash : t -> state list

val remove_unreachable : t -> t
val minimize : t -> t

val make_noncomplete : int -> bool array -> ('a * int) list array -> 'a noncomplete
val drop_noncomplete : 'a noncomplete -> int * bool array * ('a * int) list array 
val nc_minimize : 'a noncomplete -> 'a noncomplete

val nd_size : nondeterministic -> int
val nd_sigma : nondeterministic -> int

val determinize : nondeterministic -> t
val nondeterministic_of_t : t -> nondeterministic

val make : int -> bool array -> int array array -> t
val make_setstar_naive : ?k:int -> int list list -> nondeterministic
val make_nondeterministic : int list -> int list -> int list array array -> nondeterministic
val confuse_letters : nondeterministic -> sigma list -> nondeterministic
val is_safe : t -> unit
val reconnait : t -> sigma list -> bool
val generate_p : out_channel -> (sigma -> string) -> t -> int -> int -> unit
val generate_f : string ->  (sigma -> string) -> t -> int -> int -> unit
val random_automaton : int -> int -> t
val random_nondeterministic : int -> int -> nondeterministic

val complement : t -> t
val intersection : t -> t -> t
val union : t -> t -> t


val nd_epsilon : int -> nondeterministic
val nd_empty : int -> nondeterministic
val nd_star : nondeterministic -> nondeterministic
val nd_concat : nondeterministic -> nondeterministic -> nondeterministic
val nd_union : nondeterministic -> nondeterministic -> nondeterministic
val nd_letter : int -> sigma -> nondeterministic
val nd_rev : nondeterministic -> nondeterministic
