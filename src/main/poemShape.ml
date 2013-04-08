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

module DRime = Debug.OfOrderConstraint (Rime)
module DPieds = Debug.OfMetricConstraint (Pieds)
module DRecord = Debug.OfOrderConstraint (Record)
module DCreation = Debug.OfConstraint (Creation.Weak)

(* ********************* Forme habituelle *********************** *)

open Contrainte

module Classic =
  FinalConstraint
    (MergeOrderConstraint
       (MergeConstraintAndOrderConstraint
	  (MergeConstraint (Grammaire) (MergeConstraint (Singulier) (Creation.Weak) ))
	  (Rime))
       (Record)) (Pieds)

let rime i = L (L i)
let newline n = R (Pieds.Newline n)
let cesure n = R (Pieds.Cesure n)
let add s = L (R (Record.Add s))
let addNewline = add "\n"
let end_ = L (R Record.END)

(* alexandrin avec césure à l'hémistiche *)
let alexandrin idRime =
  [ 
    rime idRime;
    newline 6;
    cesure 6;
    addNewline
  ]

(* alexandrin sans forcer la césure *)
let alexandrin_smpl idRime =
  [
    rime idRime;
    newline 12;
    addNewline
  ]

(* construit le poeme *)
let poeme_alexandrins n =
  let rec aux = function
    |0 -> []
    |i -> ( alexandrin ((i + 1) / 2) ) @ ( aux (i - 1) ) in
  [addNewline; add "."]@(aux n)@[end_]

(* ************************ En vers libres *********************** *)

module Free =
  Contrainte.FinalConstraint
    (Contrainte.MergeConstraintAndOrderConstraint
       (Contrainte.MergeConstraint (Grammaire)
	  (Contrainte.MergeConstraint (Singulier) (Creation.Weak)))
       (Record)) (Libre)

let read n = R n
let add s = L (R (Record.Add s))
let addNewline = add "\n"
let end_ = L (R Record.END)

let vers_libre n = [read n; addNewline]

let poeme_libre n =
  let rec aux = function
    |0 -> []
    |i -> (vers_libre (Random.int 30))@(aux (i - 1)) in
  [ addNewline; add "." ]@( aux n )@[ end_ ]
