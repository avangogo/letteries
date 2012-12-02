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

(* algorithme général *)
let engendre get choose go x0 =
  let rec engendre0 x =
    explore x (get x)
  and explore x next0 =
    let arrow, next1 = choose next0 in
    try
      engendre0 (go x arrow)
    with
      |Contrainte.ContrainteNonRespectee -> explore x next1
  in
  engendre0 x0;;

(* 
state = position dans l'exploration
trans (transisitons) = ensemble de position à explorer depuis une position
arrow = action à faire lors d'un changement d'état

get : state -> trans
choose : trans -> arrow * trans
go : state -> arrow -> state
*)

(*type state = string * C.state
  type trans = Markov.trans
  type arrow = string * C.metadata list*)


module type Markov =
sig
  type t
  type trans
  type meta
  val get : t -> State.t -> trans
  val choose : trans -> (State.t * meta list) * trans
end;;


module Engendre ( C : Contrainte.Constraint ) ( Markov : Markov with type meta = C.metadata ) =
struct
  let write (markov : Markov.t) last_word init =
    Printf.printf "Constraint name : %s.\n" C.name;
    let get (s, _) = Markov.get markov s
    and choose = Markov.choose
    and go (_, m) (s2, l) = (s2, (List.fold_left C.step m l)) in
    try
      engendre get choose go (last_word, init)
    with
      |Record.End l -> l
end
