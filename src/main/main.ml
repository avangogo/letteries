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

(* Change here to select the constraint *)
module T = PoemShape.Noble
let poeme n = PoemShape.quatrain n (*PoemShape.poeme_alexandrins n*)


module B = Complex_bdd.ComplexBdd (T)

module E = Engendre.Engendre (T) (B);;


let construit_poeme parse_texts =
  Print.p (Printf.sprintf "Premier mot : %s" !Param.first_word);
  Print.p (Printf.sprintf "Graine : %i" !Param.seed);
  Print.p (Printf.sprintf "Nom de la contrainte : %s." T.name);

  Random.init !Param.seed;
  
  (* construction des automates de calcul de phonétique *)
  Print.p "Précalcul des règles phonétiques…";

  let t0 = Sys.time () in
  let machine = Phonetique.make_automate !Param.phoneticrules_file in
  let t1 = (Sys.time ()) -. t0 in
  Print.verbose (Printf.sprintf "%d états" (Phonetique.size machine));
  Print.verbose (Printf.sprintf "Temps de création de l’automate : %f" t1);

  (* La fonction de traduction aux modules qui l’utilisent *)
  let traduit mot = Phonetique.of_string machine (UseCamomile.latin0_of_utf8 mot) in
  Pieds.automaton := traduit;
  Rime.automaton := traduit;

  (* Lecture du corpus *)
  Print.p "Récupération des textes…";

  let textes_parses = parse_texts () in
  
  Print.p "Précalcul des données…";
  let markov = B.build textes_parses in

  let out = open_out "bdd" in
  B.printAll out markov;
  close_out out;

  Print.p "Initialisation…";
  (* Création de l’état initial *)


  let regle = poeme !Param.poemLength  in
  
  let state_init = T.make_init regle in
  Print.p "État initial accepté.";

  (* Recherche du poeme *)

  begin
    let firstWord = !Param.first_word in
    try
      let tag = List.hd (B.possibleTags markov firstWord) in
      Print.p (Printf.sprintf "Tag de %s : %s" firstWord (Tag.string_of_tag tag));

      Print.p "Écriture…";

      let poeme = E.write markov (State.make (firstWord, tag)) state_init in
      
	(* Mise en forme et affichage *)
      Print.p "Mise en page…";
      match !Param.output with
	|None -> Ecriture.affiche_poeme poeme
	|Some file ->
	  begin
	    let out = open_out file in
	    Ecriture.affiche_poeme ~out:out poeme;
	    close_out out
	  end
    with
      |Contrainte.ContrainteNonRespectee -> Print.error "Echec de l'écriture."
      |Failure "hd" -> Print.error (Printf.sprintf "%s n'est pas dans le corpus." firstWord) (* pas propre : il faudrait une erreur spécifique *)
  end;
  
    (* Affichage des modules de débuggages *)
  let debug = !Debug.mem in
  List.iter Print.p (List.rev debug)
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
