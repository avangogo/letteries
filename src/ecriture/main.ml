
(*module DRime = Debug.OfOrderConstraint (Rime)
module DPieds = Debug.OfMetricConstraint (Pieds)
module DRecord = Debug.OfOrderConstraint (Record)
module DCreation = Debug.OfConstraint (Creation)*)

module T =
  Contrainte.FinalConstraint
    (Contrainte.MergeOrderConstraint
       (Contrainte.MergeConstraintAndOrderConstraint
	  (Creation) (Rime)) (Record)) (Pieds)

(*
module T =
  Contrainte.FinalConstraint
    (Contrainte.MergeOrderConstraint
       (Contrainte.MergeConstraintAndOrderConstraint
	  (Creation) (DRime)) (Record)) (Pieds)
*)

module B = Random_bdd.RandomBdd (T)

module E = Engendre.Engendre (T) (B);;

let ps = print_string;;
let p x = ps x; print_newline ();;
let pp x = Phonetique.print x; print_newline ();;

let construit_poeme () =
  Random.self_init ();
  (* construction des automates de calcul de phonétique *)
  p "Précalcul des règles phonétiques (peut être long).";
  
  let t0 = Sys.time () in
  let machine = Phonetique.make_automate "data/reglesphonetiques" in
  let t1 = (Sys.time ()) -. t0 in
  Printf.printf "Temps de création de l'automate: %f\n" t1;

  (* La fonction de traduction aux modules qui l'utilisent *)
  let traduit = Phonetique.of_string machine in
  Pieds.automaton := traduit;
  Rime.automaton := traduit;

  (* Lecture du corpus *)
  p "Récupération des textes..."; 
  let textes_parses = Lecture.recupere_textes "data/corpus/"
    ["Poésies(Mallarmé,1914)/"; "Les fleurs du mal (1868)/"; "Rimbaud, Poésies/"] in
  
  p "Précalcul des données...";
  let markov = B.build textes_parses in
  
  (* Création de l'état initial *)
  let alexandrin rime = 
    [Contrainte.L (Contrainte.L rime);
     Contrainte.R (Pieds.Newline 6);
     Contrainte.R (Pieds.Cesure 6);
     Contrainte.L (Contrainte.R (Record.Add "\n"))] in
     
  let regle =
    List.concat
      [
	[ Contrainte.L (Contrainte.R (Record.Add "\n"));
	  Contrainte.L (Contrainte.R (Record.Add ".")) ];
	alexandrin 1;
	alexandrin 2;
	alexandrin 1;
	alexandrin 2;
	alexandrin 1;
	alexandrin 3;
	alexandrin 4;
	alexandrin 3;
	alexandrin 4;
	alexandrin 5;
	alexandrin 5;
	[ Contrainte.L (Contrainte.R Record.END) ]
      ] in
  let state_init = T.make_init regle in
  p "Etat initial accepté";
  
  try
    (* Recherche du poeme *)
    p "Ecriture...";
    let poeme = E.write markov (State.of_string Sys.argv.(1)) state_init in
    
    (* Mise en forme et affichage *)
    p "Mise en page...";
    ignore (Ecriture.affiche_poeme true poeme);

    (* Affichage des modules de débuggages *)
    let debug = !Debug.mem in
    List.iter p (List.rev debug);
      
  with Not_found -> (p "Sylvain est un pignouf") (*poème alternatif*)
;;


construit_poeme ();;
