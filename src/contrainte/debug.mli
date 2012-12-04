val mem : string list ref

module OfConstraint :
  functor (C : Contrainte.Constraint) ->
    (Contrainte.Constraint
     with type metadata = C.metadata
     and type state = C.state * string list)

module OfOrderConstraint :
  functor (O : Contrainte.OrderConstraint) ->
    (Contrainte.OrderConstraint
     with type C.metadata = O.C.metadata
     and type C.state = O.C.state * string list
     and type order = O.order)

module OfMetricConstraint :
  functor (M : Contrainte.MetricConstraint) ->
    (Contrainte.MetricConstraint
     with type C.metadata = M.C.metadata
     and type C.state = M.C.state * string list
     and type order = M.order)
