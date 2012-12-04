exception End of string list

type action = Add of string | END

include Contrainte.OrderConstraint
with type  order = action
