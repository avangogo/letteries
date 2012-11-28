type t
type el = int
type set = int

val sets : t -> int
val make : int -> t
val size : t -> set -> int
val set : t -> el -> set
val first : t -> set -> el
val next : t -> el -> el
val mark : t -> el -> unit
val split : t -> set -> set
val no_marks : t -> set -> bool
val iter : t -> (el -> unit) -> set -> unit
