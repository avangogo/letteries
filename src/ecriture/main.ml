open Grammaire
(*module DRime = Debug.OfOrderConstraint (Rime)
module DPieds = Debug.OfMetricConstraint (Pieds)
module DRecord = Debug.OfOrderConstraint (Record)
module DCreation = Debug.OfConstraint (Creation)*)

module T =
  Contrainte.FinalConstraint
    (Contrainte.MergeOrderConstraint
       (Contrainte.MergeConstraintAndOrderConstraint
	  (Contrainte.MergeConstraint (Grammaire) (Creation))
	  (Rime)) (Record)) (Pieds)

module B = Random_bdd.RandomBdd (T)

module E = Engendre.Engendre (T) (B);;

let ps = print_string;;
let p x = ps x; print_newline ();;
let pp x = Phonetique.print x; print_newline ();;

let construit_poeme parse_texts =
  Printf.printf "Graine : %i\n" !Param.seed;
  Printf.printf "Premier mot : %s\n" !Param.first_word;

  Random.init !Param.seed;
  
  (* construction des automates de calcul de phonétique *)
  p "Précalcul des règles phonétiques (peut être long).";

  let t0 = Sys.time () in
  let machine = Phonetique.make_automate !Param.phoneticrules_file in
  let t1 = (Sys.time ()) -. t0 in
  Printf.printf "Temps de création de l’automate : %f\n" t1;

  (* La fonction de traduction aux modules qui l’utilisent *)
  let traduit = Phonetique.of_string machine in
  Pieds.automaton := traduit;
  Rime.automaton := traduit;

  (* Lecture du corpus *)
  p "Récupération des textes…";

  let textes_parses = parse_texts () in
  
  p "Précalcul des données…";
  let markov = B.build textes_parses in

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

  let regle =
    List.concat
      [
	[ Contrainte.L (Contrainte.R (Record.Add "\n"));
	  Contrainte.L (Contrainte.R (Record.Add ".")) ];
	alexandrin_smpl 1;
	alexandrin_smpl 1;
	alexandrin_smpl 2;
	alexandrin_smpl 2;
	alexandrin_smpl 3;
	alexandrin_smpl 3;
	[ Contrainte.L (Contrainte.R Record.END) ]
      ] in
  let state_init = T.make_init regle in
  p "État initial accepté.";

  (* Recherche du poeme *)
  p "Écriture…";

  begin
    try
      let poeme = E.write markov (State.make (!Param.first_word, Tag.ADJ)) state_init in
      
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
      |Contrainte.ContrainteNonRespectee -> p "Echec de l'écriture."
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
