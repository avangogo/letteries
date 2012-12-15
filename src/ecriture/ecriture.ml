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

let ps = print_string
let p = fun s -> ps s; print_newline ()

(* comportement de la ponctuation vis-à-vis des espaces *)
let ponctuation_liante = ['-' ; '\'']
let ponctuation_finale = ['.'; '!'; '?'; ';'; ':'; ','; '\n' ]
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
3: ' -> 2 | , -> 1 | a -> 3  | " " -> 1

*)

let list_iteri f (*int -> 'a -> unit*) =
  let rec aux i  = function
    |t::q -> (f i t); aux (i+1) q
    |[]   -> () in
    aux 0;;

let string_of_char_list liste=
  let n = List.length liste in
  let res = String.make n 't' in
    list_iteri (fun i c -> (res.[n-i-1] <- c)) liste;
    res;;

(*fait une nouvelle liste de mot, plus propre*)
(*en réunissant les caractères devant être collés*)
let reparse l =
  let mot_en_cours = ref [] in
  let resultat = ref [] in
  let push x = mot_en_cours := x::(!mot_en_cours);
  and mot_fini () =
    let mot_lu = string_of_char_list (!mot_en_cours) in
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
;;

(*met les sut de ligne à la fin du mot*)
let gere_sauts s0 =
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
  s;;

(*écrit le poeme par "vers"*)
let met_en_page largeur l =
  let rec aux decalage (*ponctuation*) mot =
    let lmot = String.length mot in
      if decalage = 0 then
	begin
	  ps (String.capitalize mot);
	  lmot
	end
      else
	if (decalage + lmot + 1 < largeur) then
	  begin
	    ps " ";
	    ps mot;
	    decalage + lmot + 1
	  end
	else
	  begin
	    print_newline ();
	    aux 0 mot
	  end
  in
    List.fold_left aux (-1) l;;

(**)
let string_end s = match String.length s with
  |0 -> failwith "string_end: chaine vide"
  |n -> s.[n-1];;

let ajoute_majuscules =
  let rec aux maj = function
    | mot :: texte ->
      (if maj then String.capitalize mot else mot) ::
	(aux (List.mem (string_end mot) ponctuation_capitalisante) texte)
    | [] -> [] in
  aux true

(*affiche un texte déjà mis en page*)
let affiche out l =
  let aux est_debut mot =
    if not est_debut then output_string out " ";
    output_string out mot;
    (string_end mot = '\n') in
  ignore (List.fold_left aux true l);;

(*met en page et affiche*)
let affiche_poeme ?(out=stdout) poeme =
  affiche out (ajoute_majuscules (List.map gere_sauts (reparse poeme)));;
