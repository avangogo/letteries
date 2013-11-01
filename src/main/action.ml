let loadPhonetic () = 
  let rules = !Param.phoneticrules_file
  and target = !Param.phoneticautomaton_file in
  Make.load
    ~makeMessage:"Précalcul des règles phonétiques…"
    ~loadMessage:"Chargement des Règles de phonétique…"
    [rules] target (fun () -> Phonetique.make_automate rules);;

let makeClean () =
  Sys.remove !Param.phoneticautomaton_file;
  Sys.remove !Param.grammarautomaton_file;
  Sys.remove !Param.markov_file;;
