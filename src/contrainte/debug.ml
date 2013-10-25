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

let mem = ref [];;

let rouge = "\027[31m"
and vert="\027[32m"
and jaune="\027[33m"
and bleu="\027[34m"
and magenta="\027[35m"
and blanc="\027[00m"
and fin = "\027[m"

module OfConstraint (C:Contrainte.Constraint) =
struct
  type metadata = C.metadata
  type state = C.state  * string list
  let precompute w = C.precompute w
  let filter (s, _) m = C.filter s m
  let step (s, l) m =
    let new_s = C.step s m in
    let new_l = (Printf.sprintf "%s%s%s -> %s%s%s" rouge (C.print_metadata m) fin bleu (C.print_state new_s) fin )::l in
    mem := new_l;
    new_s, new_l
  let init_state () = C.init_state (), ["DEBUT"]
    (* prettyprinting *)
  let name = Printf.sprintf "[DEBUG(%s)]" C.name
  let print_metadata m = C.print_metadata m 
  let print_state (s, _) = C.print_state s
end

module OfOrderConstraint (O:Contrainte.OrderConstraint) =
struct
  module C = OfConstraint (O.C)
  type order = O.order
  let use_order (s, l) c =
    try
      let new_s = O.use_order s c in
      let new_l = (Printf.sprintf "--> %s%s%s" bleu (O.C.print_state new_s) fin)::l in
      mem := new_l;
      (new_s, new_l)
    with Record.End txt -> 
      (mem := l; raise (Record.End txt))
end

module OfMetricConstraint (M : Contrainte.MetricConstraint) =
struct
  module D = OfOrderConstraint (M)

  module C = D.C
  type order = D.order
  let use_order = D.use_order
  let finished (s, _) = M.finished s
end
