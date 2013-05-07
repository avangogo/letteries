let api = ref false;;
let latin0 = ref false;;

Arg.parse
  ["-api", Arg.Set api, "Pour avoir une sortie dans l'alphabet phonetique internationnal";
   "-latin0", Arg.Set latin0, "Pour lire l'entrée en format ISO 8859-15 (ou latin0)"]
  (fun s -> raise (Arg.Bad (Printf.sprintf "Option inconnue : %s" s)))
  "Boucle interractive pour tester le calcul de phonétique."

let automaton = Phonetique.make_automate !Param.phoneticrules_file;;

while true do
  print_string ">> ";
  let word = (if !latin0 then fun x -> x else UseCamomile.latin0_of_utf8) (read_line ()) in
  (if !api then Phonetique.prettyprint_api_utf8 else Phonetique.print) (Phonetique.of_string automaton word);
  print_newline ()
done
