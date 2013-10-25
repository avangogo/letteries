exception End of Word.word list

type action = Add of string | END

include Contrainte.OrderConstraint
with type  order = action
