type nondeterministic
type state
type sigma = int
type t

val pauto : t -> unit
val sigma : t -> int
val size : t -> int

val empty : t

val initState : t -> state
val isFinal : t -> int -> bool
val delta : t -> state -> sigma -> state

val trash : t -> state list

val remove_unreachable : t -> t
val minimize : t -> t

val nd_size : nondeterministic -> int
val nd_sigma : nondeterministic -> int
val determinize : nondeterministic -> t
val make_setstar_naive : ?k:int -> int list list -> nondeterministic
val confuse_letters : nondeterministic -> sigma list -> nondeterministic
val is_safe : t -> unit
val reconnait : t -> sigma list -> bool
val generate_p : out_channel -> (sigma -> string) -> t -> int -> int -> unit
val generate_f : string ->  (sigma -> string) -> t -> int -> int -> unit
val random_automaton : int -> int -> t
val random_nondeterministic : int -> int -> nondeterministic
