let automaton = Phonetique.make_automate !Param.phoneticrules_file;;

while true do
  print_string ">> ";
  let word = read_line () in
  Phonetique.print (Phonetique.of_string automaton word);
  print_newline ()
done
