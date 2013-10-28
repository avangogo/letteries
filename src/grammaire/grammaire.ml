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
open Word
open Parser_g

let words = ref []
let automaton = ref Finiteautomaton.empty
let trash = ref []

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
    |[] -> List.rev acc_text
    |SENT::q -> aux ((List.rev (SENT::acc))::acc_text) [] q
    |t::q -> aux acc_text (t::acc) q in
  aux [] [] l;;

let learn taglist =
  words := (cutSentences taglist) @ !words;;

type metadata = int
type state = Finiteautomaton.state

let precompute w = Tag.int_of_tag w.tag

let final s = Finiteautomaton.isFinal !automaton s

let step (state : state) id_tag =
  let new_state = Finiteautomaton.delta !automaton state id_tag in
  if List.mem new_state !trash then raise Contrainte.ContrainteNonRespectee
  else new_state

let init_state () =
  automaton :=
    begin
      if !Param.oldGrammar
      then
	begin 
	  Print.p "Grammaire : Création de l'automate…";
	  Regles_grammaires.old !words
	end
      else
	let rules = !Param.grammarrules_file
	and target = !Param.grammarautomaton_file in
	Make.load
	  ~makeMessage:"Précalcul des règles de grammaire…"
	  ~loadMessage:"Chargement des règles de grammaire…"
	  [rules] target (fun () -> Regles_grammaires.auto rules)
    end;
  Print.verbose (Printf.sprintf "%i states" (Finiteautomaton.size !automaton));    
  Print.verbose "Grammaire : Minimisation";
  automaton := Finiteautomaton.minimize !automaton;

  Print.verbose (Printf.sprintf "%i states" (Finiteautomaton.size !automaton));

  trash := Finiteautomaton.trash !automaton;
  Finiteautomaton.delta !automaton (Finiteautomaton.initState !automaton) (int_of_tag SENT)

let name = "Grammaire";;

let print_state _ = "<abstr>";;

let print_metadata id_tag = Tag.string_of_tag (Tag.tag_of_int id_tag);;
