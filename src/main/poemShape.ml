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
module DGrammaire = Debug.OfConstraint (Grammaire)
module DSingulier = Debug.OfConstraint (Singulier)

(* ********************* Forme habituelle *********************** *)

open Common
open Contrainte

module Classic =
  FinalConstraint
    (MergeOrderConstraint
       (MergeConstraintAndOrderConstraint
	  (MergeConstraint (Grammaire) (MergeConstraint (Singulier) (Creation.Weak) ))
	  (Rime))
       (Record)) (Pieds)

module Noble =
  FinalConstraint
    (MergeOrderConstraint
       (MergeConstraintAndOrderConstraint
	  (MergeConstraint (Grammaire) (MergeConstraint (Singulier) (Creation.Normal) ))
	  (Rime))
       (Record)) (Pieds)

let rime i = O (L i)
let newline n = M (Pieds.Newline n)
let cesure n = M (Pieds.Cesure n)
let add s = O (R (Record.Add s))
let addNewline = add "\n"
let end_ = O (R Record.END)

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
  [addNewline; add "."]@(aux n)@[STOP; end_]

let quatrain _ =
  List.concat
    [
      [addNewline; add "."];
      alexandrin 1;
      alexandrin 2;
      alexandrin 1;
      alexandrin 2;
      [STOP; end_]
    ]

let sonnet _ =
  List.concat
    [
      [addNewline; add "."];
      alexandrin 1;
      alexandrin 2;
      alexandrin 2;
      [addNewline];
      alexandrin 0;
      alexandrin 3;
      alexandrin 3;
      [addNewline];
      alexandrin 4;
      alexandrin 5;
      alexandrin 4;
      alexandrin 5;
      [addNewline];
      alexandrin 6;
      alexandrin 7;
      alexandrin 6;
      alexandrin 7;
      [STOP; end_]
    ]

(* ************************ En vers libres *********************** *)

module Free =
  Contrainte.FinalConstraint
    (Contrainte.MergeConstraintAndOrderConstraint
       (Contrainte.MergeConstraint (Grammaire)
	  (Contrainte.MergeConstraint (Singulier) (Creation.Weak)))
       (Record)) (Libre)

let read n = M n
let add s = O (Record.Add s)
let addNewline = add "\n"
let end_ = O Record.END

let vers_libre n = [read n; addNewline]

let poeme_libre n =
  let rec aux = function
    |0 -> []
    |i -> (vers_libre (Random.int 30))@(aux (i - 1)) in
  [ addNewline; add "." ]@( aux n )@[ STOP; end_ ]


(* *********************** Acrostiche *************************** *)

module Constraints =
  MergeConstraint (Grammaire)
    (MergeConstraint (Singulier) (Creation.Weak))

module OrderConstraint =
  MergeOrderConstraint (Rime)
    (MergeOrderConstraint (Record) (Acrostiche))

module AvecAcrostiche =
  FinalConstraint
    (MergeConstraintAndOrderConstraint
       (Constraints) (OrderConstraint))
    (Pieds)

let rime i = O (L i)
(* let newline n = R (Pieds.Newline n)
let cesure n = R (Pieds.Cesure n)*)
let add s = O (R (L (Record.Add s)))
let addNewline = add "\n"
let end_ = O (R (L Record.END))
let initiale a = O (R (R a))

let acrovers a idRime =
  [ 
    rime idRime;
    newline 12;
    initiale a;
    addNewline
  ]

let rec make_rimes l =
  let rec aux = function
    | 0 -> []
    | i -> ((i + 1)/ 2) :: (aux (i-1)) in
  aux l;;
  

let poeme_acrostiche s _ =
  let letters = List.rev (Common.list_of_string s) in
  let rimes = make_rimes (List.length letters) in
  [addNewline; add "."]
  @ (List.concat (List.map2 acrovers letters rimes))
  @ [STOP; end_];;


(* ******** Tests *************** *)
(* module DAlexandrins = Debug.OfMetricConstraint (Alexandrins)

module TestConstraints =
  MergeConstraint (Grammaire)
    (MergeConstraint (Singulier) (Creation.Normal))

module TestOrderConstraint =
  MergeOrderConstraint (Rime) (Record)

module TestPoem =
  FinalConstraint
    (MergeConstraintAndOrderConstraint
       (TestConstraints) (TestOrderConstraint))
    (Alexandrins)

let testRime i = (L (L i))
let testAdd s = L (R (Record.Add s))
let testEnd = L (R Record.END)
let testNewline = R ()

let testAlexandrin idRime =
  [
    testRime idRime;
    testNewline;
    testAdd "\n"
  ];;


let testpoem _ =
  List.concat
    [
      [testAdd ".\n"];
      testAlexandrin 1;
      testAlexandrin 1;
      testAlexandrin 2;
      testAlexandrin 2;
      [testEnd]
    ];;

*)
