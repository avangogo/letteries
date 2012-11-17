(*met un mot dans un format pratique pour compter les pieds*)
(*a ameliorer*)
let traduit_mot m =
  match m with
    |t::_ -> (t, nbre_voyelles m, rime m, fin_muette m)
    |_    -> (Phoneme.vide, 0, [], (None, None))

let tete0 =
  (0,(None,None),[]);;

let avance (n,fin0,rime0) (*tete*) (p,m,rime,fin) (*mot lu*) =
  if p = Phoneme.vide
  then (n,fin0,rime0)
  else (n+m+(nbre_voyelles (liaison fin0 p)), fin, rime);;


let traduit_string auto p =
  traduit_mot (traduit_phrase auto p);;
