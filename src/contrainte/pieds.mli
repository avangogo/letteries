val automaton : (string -> Phonetique.phoneme list) ref

type rules = Newline of int | Cesure of int

include Contrainte.MetricConstraint
with type order = rules
