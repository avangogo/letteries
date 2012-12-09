(*  Lettreries is a random poem generator.
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
    along with this program.  If not, see <http://www.gnu.org/licenses/>. *)


(*construit l'automate*)
let lis_regles fichier =
  let entree = open_in fichier in
  let res0 = Lexing.from_channel entree in
  let res1 = Parser.main Lexer.token res0 in
  close_in entree;
  res1;;

let construit_machine nom =
  let r1 = lis_regles nom in
  let r2 = Regles.construit_1 r1 in
  let r3 = Regles.construit_2 r2 in
  Traduction.precalcul r3;;

(*fait une liaison*)
let liaison (voy,con) premiere =
  if Phoneme.voyelle premiere
  then match con with
    |Some c -> (match voy with
	|Some v -> [Phoneme.exprime v; Phoneme.exprime c]
	|None   -> [Phoneme.exprime c])
    |None   -> []
  else if Phoneme.consonne premiere 
  then match voy with
    |Some v -> [Phoneme.exprime v]
    |None   -> []
  else [];;

exception MauvaisArg of Phoneme.phoneme (*string*);;

(**)
let fin_muette mot =
  match List.filter Phoneme.muet mot with
    |[a;b] -> if not (Phoneme.voyelle a && Phoneme.consonne b)
      then raise (MauvaisArg a (*,"fin muette:"*))
      else (Some a, Some b)
    |[a]   -> if Phoneme.voyelle a then (Some a, None) else (None, Some a)
    |[]    -> (None,None)
    |_     -> (Affichage.p_list Affichage.p_phoneme mot);failwith "fin muette: trop de lettres muettes";;

(*concatene deux listes de phonemes, pas tres efficace*)
let concat m1 m2 =
  match m2 with
    |[] -> m1
    |t::_ ->
      let m = List.filter (fun x -> not (Phoneme.muet x)) m1 in
      let fin = fin_muette m1 in
      m@(liaison fin t)@m2;;

let rime mot =
  let rec enleve_muet = function
    |t::q -> if Phoneme.muet t then enleve_muet q else t::q
    |[]   -> [] in
  let rec recupere_rime acc = function
    |t::q -> if Phoneme.voyelle t then t::acc else recupere_rime (t::acc) q
    |[]   -> [] (*failwith "pas de rime"*)
  in
  recupere_rime [] (enleve_muet (List.rev mot));;

let nbre_voyelles mot =
  List.fold_right (fun c i -> if (Phoneme.voyelle c)&&(not(Phoneme.muet c)) then i+1 else i) mot 0;;


let traduit_phrase auto p =
  let segments = Format.segmente p in
  (*List.iter (fun x -> print_string x; print_newline ()) segments;*)
  List.fold_left concat [] (List.map (Traduction.traduit auto) segments);;


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
