val automaton : (string -> Phonetique.phoneme list) ref

module C : Contrainte.Constraint
  with type metadata = Phonetique.phoneme * int * Phonetique.muet
  and type state = Phonetique.phoneme * int
	 
type order = Newline of int | Cesure of int
val use_order : Phonetique.phoneme * 'a -> order -> Phonetique.phoneme * int
val finished : 'a * int -> bool
