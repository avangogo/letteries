exception ContrainteNonRespectee

type ('a, 'b) sum = L of 'a | R of 'b

type ('a, 'b) finalOrder = O of 'a | M of 'b | STOP

module type Constraint =
  sig
    type metadata
    type state
    val precompute : Word.word -> metadata
    val final : state -> bool
    val step : state -> metadata -> state
    val init_state : unit -> state
    val name : string
    val print_metadata : metadata -> string
    val print_state : state -> string
  end

module type OrderConstraint =
  sig
    module C : Constraint
    type order
    val use_order : C.state -> order -> C.state
  end

module type MetricConstraint =
  sig
    include OrderConstraint
    val finished : C.state -> bool
  end

module MergeConstraint :
  functor (A : Constraint) ->
    functor (B : Constraint) -> Constraint

module MergeConstraintAndOrderConstraint :
  functor (A : Constraint) ->
    functor (B : OrderConstraint) ->
      OrderConstraint with type order = B.order

module MergeOrderConstraint :
  functor (A : OrderConstraint) ->
    functor (B : OrderConstraint) ->
      OrderConstraint with type order = (A.order, B.order) sum

module FinalConstraint :
  functor (O : OrderConstraint) ->
    functor (Metric : MetricConstraint) ->
      (sig
	include Constraint
	val make_init : (O.order, Metric.order) finalOrder list -> state
       end)
