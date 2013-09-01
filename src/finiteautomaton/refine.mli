type t
type elt = int
type set = int

val sets : t -> int
val make : int -> t
val size : t -> set -> int
val set : t -> elt -> set
val first : t -> set -> elt
val next : t -> elt -> elt
val mark : t -> elt -> unit
val split : t -> set -> set
val no_marks : t -> set -> bool
val iter : t -> (elt -> unit) -> set -> unit
