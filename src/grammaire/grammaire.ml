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
open Tag

let words = ref []
let automaton = ref Finiteautomaton.empty
let trash = ref []

let p s = Printf.printf "Grammaire : %s\n" s; flush stdout;;

(* say wether the word can be a state of the automaton *)
let isRelevant = function
  |DET _ -> false
  |PUN _ -> false
  |SENT -> false
  |PRP _ -> false
  |PRO _ -> false
  |ADV -> false
  |INT -> false
  |KON -> false
  |_ -> true;;

(* cut sentences after SENT tags *)
let cutSentences l =
  let rec aux acc_text acc = function
    |[] -> List.rev acc_text (* rev superflu: juste pour garder l'ordre du texte *)
    |SENT::q -> aux ((List.rev (SENT::acc))::acc_text) [] q
    |t::q -> aux acc_text (t::acc) q in
  aux [] [] l;;

(* function to filter sentences *)
let isAcceptable sentence =
  let l = List.length sentence in
  !Param.minSentenceLength <= l  && l <= !Param.maxSentenceLength && not (List.mem INT sentence)

let learn taglist =
  words := (cutSentences taglist) @ !words;;

let rec take_begin n l =
  if n = 0 then [] else match l with
    |t::q -> t::(take_begin (n-1) q)
    |[] -> []

type metadata = int
type state = Finiteautomaton.state

let precompute _ _ t _ = Tag.int_of_tag t

let filter _ _ = true

let step (state : state) id_tag =
  let new_state = Finiteautomaton.delta !automaton state id_tag in
  if List.mem new_state !trash then raise Contrainte.ContrainteNonRespectee
  else new_state

let init_state () =
  p "words";
  let words2 =
      (List.map List.rev
	 (List.map
	    (List.map (Tag.int_of_tag)) 
	    (List.filter isAcceptable !words))) in
  p "nd_auto";
  let nd_auto = Finiteautomaton.make_setstar_naive ~k:Tag.nbre words2 in
  p "confuse_letters"; (*allègement des regles de grammaires*)
  let conf_auto  = Finiteautomaton.confuse_letters nd_auto    (List.map int_of_tag [VER Futu; VER Impf; VER Pres; VER Simp]) in
  let conf2_auto = Finiteautomaton.confuse_letters conf_auto  (List.map int_of_tag [VER Subi; VER Subp]) in
  let conf3_auto = Finiteautomaton.confuse_letters conf2_auto (List.map int_of_tag [ADJ; VER Pper; VER Ppre]) in
  let conf4_auto = Finiteautomaton.confuse_letters conf3_auto (List.map int_of_tag [PRO (Some DEM); DET ART; DET Pos; NUM]) in
  p "determinize";
  let det_auto = Finiteautomaton.determinize conf4_auto in
  Printf.printf "%i states\n" (Finiteautomaton.size det_auto);
  p "minimize" ;
  let min_auto = Finiteautomaton.minimize det_auto in
  Printf.printf "%i states\n" (Finiteautomaton.size min_auto);
  p "starts";
  automaton := min_auto;
  trash := Finiteautomaton.trash !automaton;
  Finiteautomaton.generate_f "sentences" (fun i -> string_of_tag (tag_of_int i)) !automaton 0 5;
  Finiteautomaton.delta !automaton (Finiteautomaton.initState !automaton) (int_of_tag SENT)

let name = "Grammaire";;

let print_state _ = "<abstr>";;

let print_metadata id_tag = Tag.string_of_tag (Tag.tag_of_int id_tag);;
