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

exception ContrainteNonRespectee

type ('a, 'b) sum =
  |L of 'a
  |R of 'b;;

module type Constraint =
sig
  type metadata (*metadonnée*)
  type state (*etat*)
  val precompute : string -> bool -> string -> metadata (* fichier -> est un mot état -> mot *)
  val filter : state -> metadata -> bool
  val step : state -> metadata -> state
  val init_state: state
  (*prettyprinting*)
  val name : string
  val print_metadata : metadata -> string
  val print_state : state -> string 
end;;

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
  let precompute f b w = A.precompute f b w, B.precompute f b w
  let filter (sa, sb) (ma, mb) = (A.filter sa ma) && (B.filter sb mb)
  let step (sa, sb) (ma, mb) = (A.step sa ma), (B.step sb mb)
  let init_state = A.init_state, B.init_state
  (* prettyprinting *)
  let name = Printf.sprintf "[%s %s]" A.name B.name
  let print_metadata (ma, mb) = Printf.sprintf "(%s, %s)" (A.print_metadata ma) (B.print_metadata mb) 
  let print_state (sa, sb) = Printf.sprintf "(%s, %s)" (A.print_state sa) (B.print_state sb)
end;;

module MergeConstraintAndOrderConstraint (A:Constraint) (B:OrderConstraint) =
struct
  module C = MergeConstraint (A) (B.C)
  type order = B.order
  let use_order ((sa,sb):C.state) o = sa, B.use_order sb o
end;;

module MergeOrderConstraint (A:OrderConstraint) (B:OrderConstraint) : OrderConstraint with type order = (A.order, B.order) sum =
struct
  module C = MergeConstraint (A.C) (B.C)
  type order = (A.order, B.order) sum
  let use_order (sa,sb) = function
    |L oa -> A.use_order sa oa, sb
    |R ob -> sa, B.use_order sb ob
end;;

(*type 'b 'a patern =
  |Read of int
  |Order of 'a*)

module FinalConstraint
  (O : OrderConstraint)
  (Metric : MetricConstraint) =
struct
  module C = MergeConstraint (O.C) (Metric.C)
  type metadata = C.metadata
  type state = C.state * (((O.order, Metric.order) sum) list)
  let precompute : string -> bool -> string -> metadata = C.precompute
  let filter (s, _ : state) (m : metadata) = C.filter s m
  let rec read_order oState metricState  = function
    |(R metricOrder)::q ->
      (oState, Metric.use_order metricState metricOrder), q
    |(L oOrder)::q -> read_order (O.use_order oState oOrder) metricState q
    |[] -> failwith "read_order"
  let step (cState, l : state) (cMeta : metadata) =
    let (oState, metricState) as newState = C.step cState cMeta in
    if not (Metric.finished metricState) then (newState, l : state)
    else
      read_order oState metricState l
  let init_state = (C.init_state, [] : state)
  let make_init l = ((read_order O.C.init_state Metric.C.init_state l) : state)
  let name = C.name
  let print_metadata : metadata -> string = C.print_metadata
  let print_state ((s, l) : state) = Printf.sprintf "%s|<%d>" (C.print_state s) (List.length l)
end
