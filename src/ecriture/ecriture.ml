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

(* Remarque : tout ou une partie de ce code pourrait être récrit
de manière plus lisible avec lex *)

(* FIXME : à nettoyer *)

open Common
open Word

(*
let ps = print_string
let p = fun s -> ps s; print_newline ()*)

(* comportement de la ponctuation vis-à-vis des espaces *)
let ponctuation_liante = ['-' ; '\'']
let ponctuation_finale = ['.'; '!'; '?'; ';'; ':'; ',';'\n']
let ponctuation_espace = [' ']

(* éléments de ponctuation suivis par une majuscule *)
let ponctuation_capitalisante = ['\n'; '!'; '?'; '.']


type type_carac = 
  |PonctuationLiante
  |PonctuationFinale
  |Espace
  |Lettre;;

let type_carac_of_char c =
  if List.mem c ponctuation_liante then PonctuationLiante
  else if List.mem c ponctuation_finale then PonctuationFinale
  else if List.mem c ponctuation_espace then Espace
  else Lettre;;


(****************Mise en page*********************)
(*automate fini*)
(*etats:
*supprimer les espaces jusqu'au mot suivant et le lire comme nouveau mot 1
*supprimer les espaces jusqu'au mot suivant et coller au mot en cours 2
*lecture d'un mot 3
[ - ' ]  [. ? , ; !] [a b c]
1: ' -> 2 | , -> 1 | a -> 3,x| " " -> 1
2: ' -> 2 | , -> 1 | a -> 3  | " " -> 2
3: ' -> 2 | , -> 1 | a -> 3  | " " -> 1*)


(*fait une nouvelle liste de mot, plus propre*)
(*en réunissant les caractères devant être collés*)
(*let reparse l =
  let mot_en_cours = ref [] in
  let resultat = ref [] in
  let push x = mot_en_cours := x::(!mot_en_cours);
  and mot_fini () =
    let mot_lu = string_of_charlist (List.rev !mot_en_cours) in
      if mot_lu <> "" then resultat := mot_lu::(!resultat);
      mot_en_cours := [] in
  let transition etat c = match (etat, (type_carac_of_char c)) with
    |(_, PonctuationLiante) -> push c; 2
    |(_, PonctuationFinale) -> push c; 1
    |(1, Lettre)            -> mot_fini (); push c; 3
    |(_, Lettre)            -> push c; 3
    |(1, Espace)            -> 1
    |(2, Espace)            -> 2
    |(3, Espace)            -> 1
    |(_, Espace)            -> failwith "Automate de 'reparse': état impossible"
  in
  let etat0 = ref 1 in
  let lire c = etat0 := transition (!etat0) c in
    List.iter (function m -> String.iter lire m; lire ' ') l;
    mot_fini (); (*on finit le dernier mot*)
    List.rev (!resultat)(*le resultat est à l'envers*)
;;*)

(* met les sauts de ligne à la fin du mot *)
(* exemple : "lampadophore\n," -> "lampadophore,\n" *)
(*let gere_sauts s0 =
  let s = String.copy s0 in
  let n = String.length s in
  let swap a b =
    let aux = s.[a] in
    s.[a] <- s.[b];
    s.[b] <- aux in
  let decale a =
    for i = a to n-2 do
      swap i (i+1)
    done in
  for i = 0 to n-2 do
    if s.[i] = '\n' then decale i
  done;
  s;;*)

(**)
let string_end s = match String.length s with
  |0 -> failwith "string_end: chaine vide"
  |n -> s.[n-1];;

(*let ajoute_majuscules =
  let rec aux maj = function
    | mot :: texte ->
      (if maj then String.capitalize mot else mot) ::
	(aux (List.mem (string_end mot) ponctuation_capitalisante) texte)
    | [] -> [] in
  aux true*)

(* **nouvelle version** *)
type etat =
 |Next
 |Stick
 |Word

let rec list_make l a =
  match l with
  |0 -> []
  |n -> a::(list_make (n-1) a)

let transition etat c = match (etat, (type_carac_of_char c)) with
  |(_, n), PonctuationLiante -> (Stick, n), [c]
  |(_, n), PonctuationFinale ->
    if c = '\n'
    then (Next, n+1), []
    else (Next, n),   [c]
  |(Next, 0), Lettre       -> (Word, 0), [' '; c]
  |(Next, n), Lettre       -> (Word, 0), (list_make n '\n')@[c]
  |(_, n), Lettre          -> (Word, n), [c]
  |(Next, n), Espace       -> (Next, n), []
  |(Stick, n), Espace      -> (Stick, n),[]
  |(Word, n), Espace       -> (Next, n), [];;

let init = (Stick, 0);;

let reparse_string etat_init s =
  let res = ref []
  and etat = ref etat_init in
  for i = 0 to (String.length s) - 1 do
    let etat_suivant, out = transition !etat s.[i] in
    etat := etat_suivant;
    res := List.rev_append out !res
  done;
  !etat, string_of_charlist (List.rev !res);;


let recoupe s =
  let push k pile =
    pile := (List.assoc s.[k] sep_assoc) :: !pile in
  let i, j = ref 0, ref ((String.length s) - 1)
  and left, right = ref [], ref [] in
  while 0 < !j && List.mem_assoc s.[!j] sep_assoc do
    push !j right;
    decr j
  done;
  while !i < !j && List.mem_assoc s.[!i] sep_assoc do
    push !i left;
    incr i
  done;
  (List.rev !left, String.sub s !i (!j - !i + 1), !right);;

let print_end (_, n) =
  list_make n (token_of_sep Newline);;

let reparse_words l =
  let rec aux acc state = function
    | w :: q ->
      let new_state, token0 = reparse_string state (" "^w.word) in
      let left, token, right = recoupe token0 in
      let new_acc = List.concat
	[ List.rev_map token_of_sep right;
	  if token = ""
	  then []
	  else [token_of_word (Word.set_word w token)];
	  List.rev_map token_of_sep left;
	  acc] in
      aux new_acc new_state q
    | [] -> List.rev (List.rev_append (print_end state) acc) in
  aux [] init l;;

(* usine à gaz.. *)
let capitalize l =
  let rec aux acc maj = function
    | (L Space) as t :: q -> aux (t::acc) maj q
    | (L Newline) as t :: q -> aux (t::acc) true q
    | (R w) :: q ->
      let t =
	R (if maj then Word.set_word w (String.capitalize w.word) else w) in
      aux (t::acc) (List.mem (string_end w.word) ponctuation_capitalisante) q
    | [] -> List.rev acc in
  aux [] true l;;


let reparse l =
  capitalize (reparse_words l)
