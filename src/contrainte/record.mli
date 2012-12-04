exception End of string list

module C : Contrainte.Constraint with type metadata = string
				 and type state = string list

type order = Add of string | END
val use_order : string list -> order -> string list
