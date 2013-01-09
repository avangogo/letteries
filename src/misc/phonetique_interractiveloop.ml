let api = ref false;;

Arg.parse
  ["-api", Arg.Set api, "Pour avoir une sortie dans l'alphabet phonetique internationnal"]
  (fun s -> raise (Arg.Bad (Printf.sprintf "Option inconnue : %s" s)))
  "Boucle interractive pour tester le calcul de phonétique."

let automaton = Phonetique.make_automate !Param.phoneticrules_file;;

while true do
  print_string ">> ";
  let word = read_line () in
  (if !api then Phonetique.prettyprint_api_utf8 else Phonetique.print) (Phonetique.of_string automaton word);
  print_newline ()
done
