val automaton : (string -> Phonetique.phoneme list) ref

val minSize : int

type key = int

include Contrainte.OrderConstraint with type order = key
