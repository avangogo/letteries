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

let p x = print_string x; print_newline ();;
let pi x = p (string_of_int x);;

let rec supprime_identique = function
  |a::b::q -> if a=b
    then supprime_identique (b::q)
    else a::(supprime_identique (b::q))
  |[a]     -> [a]
  |[]      -> [];;

let rec liste_suffixe = function
  |t::q -> (t::q)::(liste_suffixe q)
  |[]   -> [[]];;

let liste_prefixe l =
  List.map List.rev (liste_suffixe (List.rev l));;

let liste_etat mots =
  let m0 = List.concat (List.map liste_prefixe mots) in
  let m1 = List.sort compare m0 in
    supprime_identique m1;;

let list_mapi f =
  let rec aux i = function
    |t::q -> (f i t)::(aux (i+1) q)
    |[]   -> [] in
  aux 0;;
    

(**********objets de l'interface*********)
type etat = int;;
type 'a t    = ('a * int array) list * 'a list array*int;;

let make alphabet mots =
  let l = liste_etat mots in
  let nb_etat = List.length l in
    (*fonctions de passage entre le numero de l'état et le mot représenté*)
  let mot_of_int = Array.of_list l
  and int_of_mot = list_mapi (fun i a -> (a,i)) l in
  
  let rec plus_grand_suffixe mot = (*renvoie un etat*)
    try
      List.assoc mot int_of_mot
    with
      |Not_found ->  plus_grand_suffixe (List.tl mot) in
  
  let transition c etat =
    let mot = mot_of_int.(etat) in
    plus_grand_suffixe (mot@[c]) in
  let construit_transitions_char c =
    let res = Array.make nb_etat (-1) in
    for i = 0 to nb_etat-1 do
      res.(i) <- transition c i;
    done;
    res in
  
  let table_transition = List.map (fun c -> (c,(construit_transitions_char c))) alphabet in
  
  let assoc_mots_initiaux =
    let rec pgsuffixe = function
      |_::q as l -> if List.mem l mots then l else pgsuffixe q
      |[]        -> [] in
    Array.map pgsuffixe mot_of_int in
  
  (table_transition, assoc_mots_initiaux, nb_etat);;

let initial = 0;;

exception Mauvais_argument of string;;

let transition automate etat c =
  try
    match automate with (table,_,_) ->
      (List.assoc c table).(etat)
  with _ -> raise (Mauvais_argument "transition");;

let traduit_etat automate etat =
  try
    match automate with (_,assoc_mots_initiaux,_) ->
      assoc_mots_initiaux.(etat)
  with _ -> raise (Mauvais_argument "traduit_etat");;

let length automate =
  match automate with (_,_,n) -> n;;

(*****tests*****)
let mots = [['c';'h';'a';'r';'l';'o';'t';'t';'e'];['m';'o';'t'];['c';'h';'a';'t'];['l';'o';'t']];;
let alphabet = ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z'];;
let a = make alphabet mots;;

traduit_etat a (List.fold_left (transition a) initial ['c';'h';'a';'r';'l';'o';'t']);;
