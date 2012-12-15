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

open Grammaire
(*module DRime = Debug.OfOrderConstraint (Rime)
module DPieds = Debug.OfMetricConstraint (Pieds)
module DRecord = Debug.OfOrderConstraint (Record)
module DCreation = Debug.OfConstraint (Creation)*)


module T =
  Contrainte.FinalConstraint
    (Contrainte.MergeOrderConstraint
       (Contrainte.MergeConstraintAndOrderConstraint
	    ((Contrainte.MergeConstraint (Grammaire) (Creation.Weak)))
	  (Rime)) (Record)) (Pieds)

module B = Complex_bdd.ComplexBdd (T)

module E = Engendre.Engendre (T) (B);;

let ps = print_string;;
let p s = if !Param.verbose then begin ps s; print_newline () end
(* let pp x = Phonetique.print x; print_newline ();; *)

let construit_poeme parse_texts =
  p (Printf.sprintf "Graine : %i" !Param.seed);
  p (Printf.sprintf "Premier mot : %s" !Param.first_word);

  Random.init !Param.seed;
  
  (* construction des automates de calcul de phonétique *)
  p "Précalcul des règles phonétiques (peut être long).";

  let t0 = Sys.time () in
  let machine = Phonetique.make_automate !Param.phoneticrules_file in
  let t1 = (Sys.time ()) -. t0 in
  p (Printf.sprintf "Temps de création de l’automate : %f" t1);

  (* La fonction de traduction aux modules qui l’utilisent *)
  let traduit = Phonetique.of_string machine in
  Pieds.automaton := traduit;
  Rime.automaton := traduit;

  (* Lecture du corpus *)
  p "Récupération des textes…";

  let textes_parses = parse_texts () in
  
  p "Précalcul des données…";
  let markov = B.build textes_parses in

  let out = open_out "bdd" in
  B.printAll out markov;
  close_out out;

  p "Initialisation…";
  (* Création de l’état initial *)
  let alexandrin rime =
    [ Contrainte.L (Contrainte.L rime);
     Contrainte.R (Pieds.Newline 6);
     Contrainte.R (Pieds.Cesure 6);
     Contrainte.L (Contrainte.R (Record.Add "\n"))] in
  let alexandrin_smpl rime =
    [ Contrainte.L (Contrainte.L rime);
      Contrainte.R (Pieds.Newline 12);
      Contrainte.L (Contrainte.R (Record.Add "\n"))] in

  let poeme_alexandrins n =
    let rec aux = function
      |1 -> []
      |i -> ( alexandrin_smpl (i / 2) ) @ ( aux (i - 1) ) in
    List.concat
      [
	[ Contrainte.L (Contrainte.R (Record.Add "\n"));
	  Contrainte.L (Contrainte.R (Record.Add ".")) ];
	( aux (n + 1) );
	[ Contrainte.L (Contrainte.R Record.END) ]
      ] in

  let regle = poeme_alexandrins !Param.poemLength  in
  
  let state_init = T.make_init regle in
  p "État initial accepté.";

  (* Recherche du poeme *)

  begin
    let firstWord = !Param.first_word in
    try
      let tag = List.hd (B.possibleTags markov firstWord) in
      p (Printf.sprintf "Tag de %s : %s\n" firstWord (Tag.string_of_tag tag));

      p "Écriture…";

      let poeme = E.write markov (State.make (firstWord, tag)) state_init in
      
	(* Mise en forme et affichage *)
      p "Mise en page…";
      match !Param.output with
	|None -> Ecriture.affiche_poeme poeme
	|Some file ->
	  begin
	    let out = open_out file in
	    Ecriture.affiche_poeme ~out:out poeme;
	    close_out out
	  end
    with
      |Contrainte.ContrainteNonRespectee -> ps "Echec de l'écriture."
      |Failure "hd" -> ps (Printf.sprintf "%s n'est pas dans le corpus." firstWord) (* pas propre : il faudrait une erreur spécifique *)
  end;
  
    (* Affichage des modules de débuggages *)
  let debug = !Debug.mem in
  List.iter p (List.rev debug)
;;

let main () =
  Param.parse_arg ();

  match !Param.task with
    |Param.PoemFromComputed -> construit_poeme
      (fun () ->
	Lecture.getComputed [!Param.computed_dir])
    |Param.PoemFromCorpus -> construit_poeme  
      (fun () ->
	Lecture.getRaw
	  (List.map ((^) !Param.corpus_dir) !Param.corpus_subdirs))
    |Param.MakeComputed -> Makecorpus.main ();;

main ();;
