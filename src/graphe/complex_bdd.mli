module ComplexBdd : functor (C : Contrainte.Constraint) ->
sig
  type t
  type trans
  type meta = C.metadata

  val frequency : t -> State.t -> int

  val sortByFrequency : t -> unit

  val build : (string * (State.wild * State.wild list) list) list -> t

  val get : t -> State.t -> trans

  val choose : trans -> (State.t * C.metadata list) * trans

  val filter : (State.t -> bool) -> t -> t

  val printStatesByArrity : out_channel -> ?min:int -> ?max:int -> t -> unit

  val printAll : out_channel -> t -> unit

  val possibleTags : t -> string -> Tag.tag list
end
