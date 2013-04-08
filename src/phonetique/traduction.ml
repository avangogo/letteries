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

open Regles;;
open Transducteur;;

type automate = (char, Phoneme.phoneme) transducer *
    (string, Phoneme.phoneme list) Hashtbl.t (* table des exceptions *)

(* type automate =
    char Automate.t * (* premier automate *)
      char Automate.t * (* second automate *)
      (char * Phoneme.phoneme list array array) list * (* fonction de résolution *)
      (string, Phoneme.phoneme list) Hashtbl.t (* table des exceptions *)

let rec est_prefixe mot pre =
  match pre with
    |[] -> true
    |t1::q1 ->
      match mot with
	|[] -> false
	|t2::q2 -> (t1=t2)&&(est_prefixe q2 q1);;

let est_suffixe mot suf =
  est_prefixe (List.rev mot) (List.rev suf);;

let pre_et_suf_fixes regles =
  let extrait_contexte = function
    |Fleche2(c1,c2,_) -> (c1,c2) in
  List.split(List.concat(List.map (fun (_,l) -> List.map extrait_contexte l) regles));;*)

exception Cassansregle of (char list)*(char list);;
exception Toto of char;;

(*let string_of_charlist l =
  let n = List.length l in
  let res = String.make n '*' in
  for i = 0 to n-1 do
    res.[i] <- List.nth l i
  done;
  res;;
let ps = print_string;;

let filtre_suffixe suf regle =
  let aux = function
    |Fleche2(_, c2, _) -> est_prefixe suf c2 in
  List.filter aux regle;;

let applique_regle_prefixe regle pre =
  let rec aux = function
    |(Fleche2(c1,_,p))::q -> 
      if (est_suffixe pre c1) then p
      else aux q
    |[] -> raise (Cassansregle (pre,[])) in
  aux regle;;*)


let table_des_exceptions assoc =
  let res = Hashtbl.create (List.length assoc) in
  List.iter (fun (mot, phon) -> Hashtbl.add res mot phon) assoc;
  res;;

let precalcul (regles, exceptions) =
  let automate = Transducteur.make (caractere_fin::Regles.alphabet) regles
  and tableexceptions = table_des_exceptions exceptions in
  (automate, tableexceptions)

(*cree tout le matériel pour permettre la traduction*)
(*let precalcul (regle, exceptions) =
  let (prefixes,suffixes) = pre_et_suf_fixes regle in
  let auto1 = Automate.make (caractere_fin::Regles.alphabet) prefixes
  and auto2 = Automate.make (caractere_fin::Regles.alphabet) (List.map List.rev suffixes) in
  
  let n1, n2 = Automate.length auto1, Automate.length auto2 in
  
  let table_char c =
    let table = Array.make_matrix n1 n2 [] in 
    let regle_c = List.assoc c regle in
    for j = 0 to n2-1 do
      let regle_c_p = filtre_suffixe (List.rev (Automate.traduit_etat auto2 j)) regle_c in
      for i = 0 to n1-1 do
	table.(i).(j) <- applique_regle_prefixe regle_c_p
	  (Automate.traduit_etat auto1 i)
      done
    done;
    table in
  let grandetable = List.map (fun c -> (c,table_char c)) alphabet in
  let tableexception = table_des_exceptions exceptions in
  (auto1,auto2,grandetable,tableexception);;*)

(**)
(* let lis_table table c i j =
  (List.assoc c table).(i).(j);;

let chartab_of_string s =
  let n = String.length s in
  let res = Array.make n '*' in
  for i = 0 to n-1 do
    res.(i) <- s.[i]
  done;
  res;;*)

(*traduit un mot*)
(* let traduit (auto1,auto2,table,exceptions) mot0 =
  try
    Hashtbl.find exceptions mot0
  with Not_found ->
    begin
      let mot = chartab_of_string mot0 in
      let n = Array.length mot in
      let etat1 = Array.make n (-1)
      and etat2 = Array.make n (-1) in
      let e1 = ref Automate.initial in (*l'état que l'on lit*)
      for i = 0 to n-1 do
	etat1.(i) <- !e1;
	e1 := Automate.transition auto1 (!e1) mot.(i)
      done;
      let e2 = ref (Automate.transition auto2 Automate.initial caractere_fin) in (*idem*)
      for i = n-1 downto 0 do
    etat2.(i) <- !e2;
	e2 := Automate.transition auto2 (!e2) mot.(i)
      done;
      List.concat (Array.to_list
		     (Array.mapi
			(fun i c -> 
			  lis_table table c etat1.(i) etat2.(i))
			mot))
    end;;*)

let charlist_of_string s =
  let n = String.length s in
  let res = ref [caractere_fin] in
  for i = n-1 downto 0 do
    res := s.[i] :: !res
  done;
  !res;;

let traduit (auto, exceptions) mot =
  try
    Hashtbl.find exceptions mot
  with Not_found ->
    transduce auto (charlist_of_string mot) (* enlever la sérialisation? *)
