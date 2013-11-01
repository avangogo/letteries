(* 
   Lettreries is a random poem generator.
    Copyright (C) 2013 Rémi de Verclos

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
let poeme n = PoemShape.poeme_alexandrins n (*PoemShape.quatrain n; PoemShape.poeme_alexandrins n*)


module B = Complex_bdd.ComplexBdd (T)

module E = Engendre.Engendre (T) (B);;


let construit_poeme reps_corpus parse_texts =
  Print.p (Printf.sprintf "Premier mot : %s" !Param.first_word);
  Print.p (Printf.sprintf "Graine : %i" !Param.seed);
  Print.p (Printf.sprintf "Nom de la contrainte : %s." T.name);

  Random.init !Param.seed;
  
  (* construction des automates de calcul de phonétique *)

  let t0 = Sys.time () in
  let machine = Action.loadPhonetic () in
  let t1 = (Sys.time ()) -. t0 in
  Print.verbose (Printf.sprintf "%d états" (Phonetique.size machine));
  Print.verbose (Printf.sprintf "Temps de création de l’automate : %f" t1);

  (* La fonction de traduction en phonetique *)
  let traduit mot = Phonetique.of_string machine (UseCamomile.latin0_of_utf8 mot) in

  (* Fonction de lecture du corpus *)
  let build_corpus () =
    Print.p "Récupération des textes…";
    let textes_parses = parse_texts traduit in
 
    Print.p "Précalcul…";
    let markov0 = B.build textes_parses in

    let forbiddenWords = ["a"; "ont"; "est"; "sont"] in
    let markov = B.filter
      (fun x -> let s, _ = Obj.magic x in
		not (List.mem s forbiddenWords))
      markov0 in
    markov in

  (* spécification des depandances*)
  let dep_corpus = List.concat (List.map Lecture.getFiles reps_corpus) in
  let dep_phonetique = [!Param.phoneticrules_file] in
  let dep_grammaire = [!Param.grammarrules_file] in
  let dep_lettreries = [Sys.executable_name] in
  let dep = List.concat
    [ dep_phonetique; dep_grammaire; dep_lettreries; dep_corpus ] in
  
  let markov = Make.load
    ~makeMessage:"Précalcul des données…"
    ~loadMessage:"Chargement des données…"
    dep !Param.markov_file build_corpus in

  (*
  let out = open_out "bdd" in
  B.printAll out markov;
  close_out out;
  *)

  (*
  let out = open_out "bdd_byArrity" in
  B.printStatesByArrity out markov;
  close_out out;*)

  (*
  let out = open_out "bdd_Marshal" in
  Marshal.to_channel out markov [];
  close_out out;
  *)

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

      let poeme_words = E.write markov (State.make (firstWord, tag)) state_init in
      
      (* Mise en forme et affichage *)
      Print.p "Mise en page…";
      let poeme_tokens = Ecriture.reparse poeme_words in

      let poeme =
	if !Param.xml
	then
	  begin (* FIXME *)
	    List.concat [
	      [Xml.prelude];
	      List.map Xml.xml_of_token poeme_tokens;
	      [Xml.epilogue]
	    ]
	  end
	else List.map Word.string_of_token poeme_tokens in

      match !Param.output with
      |None -> List.iter print_string poeme
      |Some file ->
	begin
	  let out = open_out file in
	  List.iter (output_string out) poeme;
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
    |Param.PoemFromComputed ->
      let corpus = [!Param.computed_dir] in 
      construit_poeme corpus
	(fun traduit ->
	  Lecture.getComputed traduit corpus)
    |Param.PoemFromCorpus ->
      let corpus = List.map ((^) !Param.corpus_dir) !Param.corpus_subdirs in 
      construit_poeme corpus
	(fun traduit ->
	  Lecture.getRaw traduit corpus
	)
    |Param.MakeComputed -> Makecorpus.main ()
    |Param.Clean -> Action.makeClean ();;


main ();;
Print.verbose (Printf.sprintf "\nTemps d'exécution : %f s." (Sys.time ()));;
