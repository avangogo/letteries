val mem : string list ref

module OfConstraint :
  functor (C : Contrainte.Constraint) -> Contrainte.Constraint

module OfOrderConstraint :
  functor (O : Contrainte.OrderConstraint) ->
    Contrainte.OrderConstraint with type order = O.order

module OfMetricConstraint :
  functor (M : Contrainte.MetricConstraint) ->
    (Contrainte.MetricConstraint with type order = M.order)
