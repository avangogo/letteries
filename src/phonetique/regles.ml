(*  Lettreries is a random poem generator.
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
    along with this program.  If not, see <http://www.gnu.org/licenses/>. *)

open Phoneme;;

(******types******)
(*type brut*)
type lettre0 =
  |Var0 of char
  |Lettre0 of char
  |Fin0;;

type regle0 =
  |Fleche0 of (lettre0 list)*(lettre0 list)*(lettre0 list)*(phoneme list);;

(**après la destruction des Var**)
type lettre1 =
  |Lettre1 of char
  |Fin1

type regle1 =
  |Fleche1 of (lettre1 list)*lettre1*(lettre1 list)*(phoneme list);;

(*type près à etre optimisé*)
(*type lettre2 = char*) (*Fin sera un caractere spécial*)

type regle2 =
  |Fleche2 of ((*lettre2*)char list)*((*lettre2*) char list)*(phoneme list);;

let caractere_fin = 'F'

(****************************************************************)
exception ErreurDeRegle of string;;

let alphabet = ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z';'\233'; '\232'];;

let string_of_char x = let s = " " in s.[0] <- x ; s;;

(**vérifications**)
let verifie_char x =
  if not (List.mem x alphabet) then raise  (ErreurDeRegle ("char: caractere inconnu : "^(string_of_char x)));;

let verifie_Lettre0 = function
  |Lettre0 c -> verifie_char c
  |_ -> raise (ErreurDeRegle "cet élément doit etre un Lettre0");;

let verifie_lettre0 vars = function
  |Var0 c    -> if not (List.mem c vars) then raise  (ErreurDeRegle ("lettre0: caractere inconnu : "^(string_of_char c)))
  |Lettre0 c -> verifie_char c
  |Fin0      -> ();;

let verifie_phoneme p =
  if not (Phoneme.est_valide1 p) then raise (ErreurDeRegle "Phoneme inconnu");;

let verifie_regle0 vars = function
  |Fleche0(c1,x,c2,p) ->
     List.iter (verifie_lettre0 vars) c1;
     List.iter (verifie_lettre0 vars) x;
     List.iter (verifie_lettre0 vars) c2;
     List.iter verifie_phoneme p;;

let verifie_exception (mot, p) =
  List.iter verifie_phoneme p;;

(*vérifie que touts les éléments invoqués existent*)
let ast0 (var,regle,expt) =
  let (vars, lettres) = List.split var in
    List.iter (List.iter verifie_Lettre0) lettres;
    List.iter (verifie_regle0 vars) regle;
    List.iter verifie_exception expt;;

(**********passage de 0 à 1*********)
let lettre1_of_lettre0 = function
  |Lettre0 c -> Lettre1 c
  |Fin0      -> Fin1
  |Var0 c    -> raise (ErreurDeRegle "lettre1_of_lettre0")

(*multiplie les petits pains*)
(*donne toutes les interprétations possibles d'une lettre*)
let lettre1list_of_lettre0 vars = function
  |Var0 c    -> List.map lettre1_of_lettre0 (List.assoc c vars)
  |Lettre0 c -> [Lettre1 c]
  |Fin0      -> [Fin1]

(*a [A1; ;An] associe A1*..*An au sens du produit cartésien*)
let rec produitcartesien = function
  |[] -> [[]]
  |t::q -> List.concat (List.map (fun l -> List.map (fun x -> x::l) t) (produitcartesien q));;


(*donne toutes les interprétations possibles d'un mot dans lettre0* *)
let lettre1listlist_of_lettre0list vars mot =
  produitcartesien (List.map (lettre1list_of_lettre0 vars) mot);;

(*construit les regles pour un contexte fixé et un mot d'éventuellement plusieurs lettre*)
let rec construit_regles1 c1 x c2 p = match x with
  |[]   -> []
  |t::q -> (Fleche1(c1,t,q@c2,p))::(construit_regles1 (c1@[t]) q c2 []);;


(*transforme une regle en toutes ses interprétations*)
let regle1_list_of_regle0 vars = function
  |Fleche0(c1,x,c2,p) -> (*(lettre0 list)*(lettre0 list)*(lettre0 list)*(phoneme list) *)
     let lc1 = lettre1listlist_of_lettre0list vars c1
     and lc2 = lettre1listlist_of_lettre0list vars c2 in
     let lx  = lettre1listlist_of_lettre0list vars x in
       (*BOUM*)
       List.concat (List.concat( List.concat(
				   List.map (fun co1 -> List.map (fun xo -> List.map (fun co2 ->
											construit_regles1 co1 xo co2 p
										     ) lc2) lx) lc1)));;
(*transforme une liste de regles*)
let construit_1 (vars,rgle,expt) =
  ast0 (vars,rgle,expt);
  ((List.concat (List.map (regle1_list_of_regle0 vars) rgle)), expt);;

(***de 1 à 2***)
let lettre2_of_lettre1 = function
  |Lettre1 c -> c
  |Fin1      -> caractere_fin;;

let rec add_assoc clef elem = function
  |(clef0,l)::q ->
    if clef=clef0 then (clef0, elem::l)::q
    else (clef0,l)::(add_assoc clef elem q)
  |[] -> [(clef,[elem])];;

let add_regle regle1 l_assoc =
  match regle1 with
      Fleche1(c1,a,c2,p) ->
	add_assoc
	  (lettre2_of_lettre1 a)
	  (Fleche2(List.map lettre2_of_lettre1 c1, List.map lettre2_of_lettre1 c2,p))
	  l_assoc;;


let regles2_assoc_of_regle1_list r1 =
  List.fold_right
    add_regle
    r1 [(caractere_fin,[Fleche2([],[],[])])];;

let construit_2 (rgle,expt) =
  (regles2_assoc_of_regle1_list rgle, expt);;
