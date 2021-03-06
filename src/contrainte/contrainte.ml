(* 
   Lettreries is a random poem generator.
    Copyright (C) 2012 Rémi de Verclos

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)
open Common

exception ContrainteNonRespectee

type ('a, 'b) finalOrder =
  |O of 'a
  |M of 'b
  |STOP

module type Constraint =
sig
  type metadata (*metadonnée*)
  type state (*etat*)
  val precompute : Word.word -> metadata
  val final : state -> bool
  val step : state -> metadata -> state
  (* compute the initial state AND initiate the automaton *)
  (* constraints can assume that init_state is called exctly once at the begining of each computation *)
  val init_state: unit -> state
  (*prettyprinting*)
  val name : string
  val print_metadata : metadata -> string
  val print_state : state -> string 
end

module type OrderConstraint =
sig
  module C : Constraint
  type order (*consigne*)
  val use_order: C.state -> order -> C.state
end;;

module type MetricConstraint =
sig
  module C : Constraint
  type order
  val use_order : C.state -> order -> C.state
  val finished : C.state -> bool
end

(*merge*)
module MergeConstraint (A:Constraint) (B:Constraint) =
struct
  type metadata = A.metadata * B.metadata
  type state = A.state*B.state
  let precompute w = A.precompute w, B.precompute w
  let final (sa, sb) = A.final sa && B.final sb  
  let step (sa, sb) (ma, mb) = (A.step sa ma), (B.step sb mb)
  let init_state () = A.init_state (), B.init_state ()
  (* prettyprinting *)
  let name = Printf.sprintf "[%s %s]" A.name B.name
  let print_metadata (ma, mb) = Printf.sprintf "(%s, %s)" (A.print_metadata ma) (B.print_metadata mb) 
  let print_state (sa, sb) = Printf.sprintf "(%s, %s)" (A.print_state sa) (B.print_state sb)
end

module MergeConstraintAndOrderConstraint (A:Constraint) (B:OrderConstraint) =
struct
  module C = MergeConstraint (A) (B.C)
  type order = B.order
  let use_order ((sa,sb):C.state) o = sa, B.use_order sb o
end

module MergeOrderConstraint (A:OrderConstraint) (B:OrderConstraint) =
struct
  module C = MergeConstraint (A.C) (B.C)
  type order = (A.order, B.order) sum
  let use_order (sa,sb) = function
    |L oa -> A.use_order sa oa, sb
    |R ob -> sa, B.use_order sb ob
end

module FinalConstraint
  (O : OrderConstraint)
  (Metric : MetricConstraint) =
struct
  module C = MergeConstraint (O.C) (Metric.C)
  type metadata = C.metadata
  type state = C.state * (((O.order, Metric.order) finalOrder) list)
  let precompute = C.precompute
  let final (s, _) = C.final s (* a priori inutile *)
  let rec read_order oState metricState  = function
    |(M metricOrder)::q ->
      (oState, Metric.use_order metricState metricOrder), q
    |(O oOrder)::q -> read_order (O.use_order oState oOrder) metricState q
    |STOP::q -> if O.C.final oState (* flou: C.final ou O.C.final ? *)
      then read_order oState metricState q
      else raise ContrainteNonRespectee
    |[] -> failwith "read_order"
  let step (cState, l : state) (cMeta : metadata) =
    let (oState, metricState) as newState = C.step cState cMeta in
    if not (Metric.finished metricState) then (newState, l : state)
    else
      read_order oState metricState l
  let init_state () = (C.init_state (), [] : state)
  let make_init l = ((read_order (O.C.init_state ()) (Metric.C.init_state ()) l) : state)
  let name = C.name
  let print_metadata : metadata -> string = C.print_metadata
  let print_state ((s, l) : state) = Printf.sprintf "%s|<%d>" (C.print_state s) (List.length l)
end
