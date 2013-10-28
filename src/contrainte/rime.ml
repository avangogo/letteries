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

open Word


(* parametres *)
let minSize = 4 (* taille minimale des mots autorisés à fournir une rime *) 

(* gestion grossière des rimes marculines/féminines. a mettre en foncteur?*)
type genre =
  |Masculine
  |Feminine
  |Vide
let string_of_genre = function
  |Masculine -> "M"
  |Feminine -> "F"
  |Vide -> "N"
      
type rime = Phonetique.rime * genre
let makeRime w =
  let genre =
    (try
       let l = String.length w.word in
       if String.sub w.word (l-1) 1 = "e"
       || String.sub w.word (l-2) 2 = "es" then Feminine
       else Masculine
     with
       | Invalid_argument _ -> Masculine) in
  let rime = Phonetique.rime w.phonetic in
  rime, genre;;
let rimeVide = Phonetique.rime_vide, Vide;;
let string_of_rime (p, genre) =
  Printf.sprintf "%s-%s" (Phonetique.string_of_rime p) (string_of_genre genre);;
(* *)  

(* le module, de type Constraint *)
type key = int
type task =
  |None_ (* ne rien faire *)
  |Record of key (* enregistrer sous 'key' la prochaine rime *)
  |Match of rime * key * (string list) (* vérifier la prochaine rime *)

module C =
struct

  type metadata = rime * string;;

  (* la rime attendue, *)
  type state = task * (key * rime * (string list)) list;;

  let precompute w =
    let rime =
      if (String.length w.word >= minSize) && w.relevant && w.phonetic <> []
      then makeRime w
      else rimeVide in
    let s = if w.phonetic = [] then "" else w.lemma in
    rime, s

  let final _ = true

  let step ((task, assoc) as state) (rime, word) =
    if word = "" then state
    else match task with
      |None_ -> state
      |Record key ->
	if rime = rimeVide then raise Contrainte.ContrainteNonRespectee
	else (None_, (key, rime, [word])::assoc)
      |Match( matchRime, key, forbidden ) ->
	if rime <> matchRime || (List.mem word forbidden) then raise Contrainte.ContrainteNonRespectee
	else (None_, (key, matchRime, (word::forbidden))::assoc)

  let init_state () = None_, [];;

  (*prettyprinting*)
  let name = "Rime";;
  let print_state (task, _) = (match task with
    | None_ -> "None"
    | Record key -> Printf.sprintf "Record %d" key
    | Match( rime, key, forbid ) -> Printf.sprintf "Match %s, %d, <%d>" (string_of_rime rime) key (List.length forbid));;
  
  let print_metadata (rime, s) = Printf.sprintf "%s: %s" s (string_of_rime rime);;
end  

type order = key

let extract key l =
  let rec aux acc = function
    |((k, _, _) as t)::q ->
      if k = key
      then List.rev_append acc q, t
      else aux (t::acc) q
    |[] -> raise Not_found in
  aux [] l;;

let use_order (_, assoc) key =
  try
    let newAssoc, (_, rime, forbid) = extract key assoc in
    Match( rime, key, forbid ), newAssoc
  with
    | Not_found -> Record key, assoc;;
