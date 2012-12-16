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

(*type principal*)
type phoneme = char*char;;

let of_string s =  match String.length s with
  |1 -> (s.[0],' ')
  |2 -> (s.[0],s.[1])
  |_ -> failwith ("phoneme_of_string: cet élément n'a pas la bonne taille: "^s);;

let to_string = function
  |(a,b) -> Printf.sprintf "'%c%s'" a (if b = ' ' then "" else String.make 1 b)

let liste =
  let brut = ["a"; "a~"; "s^"; "n~"; "e"; "e~"; "v"; "i"; "o~"; "z"; "u"; "x~"; "z^"; "y"; "p"; "j"; "o"; "t"; "w"; "x"; "k"; "h"; "q"; "b"; "l"; "a^"; "d"; "r"; "x^"; "g"; "m"; "e^"; "f"; "n"; "o^"; "s"; "g~"] in
    List.map of_string brut;;

let est_valide x =
  List.mem x liste;;

let p1 = function (x,_) -> x
and p2 = function (_,y) -> y;;

let voyelle x =
  let liste = ['a';'e';'i';'o';'u';'y';'x';'q'] in
    List.mem (p1 x) liste;;

let consonne x =
  let liste = ['b';'d';'f';'g';'h';'j';'k';'l';'m';'n';'p';'r';'s';'t';'v';'w';'z'] in
    List.mem (p1 x) liste;;

let muet x =
  p2 x = '_';;

let exprime = function
  |(x,'_') -> (x,' ')
  |(_,  _) -> failwith "exprime: lettre non muette";;

(*les éléments 0 sont les terminaisons muettes*)
(* les élément 1 sont ceux parsés, avec les deux*)
let of_string0 s =  match String.length s with
  |1 -> (s.[0],'_')
  |_ -> failwith ("phoneme_of_string: cet élément n'a pas la bonne taille: "^s);;

let liste0 =
  let brut = ["q"; "b"; "d"; "m"; "n"; "p"; "r"; "s"; "t"; "z"] in
  (List.map of_string0 brut);;

let liste1 = liste@liste0;;

let est_valide0 x =
  List.mem x liste0;;

let est_valide1 x =
  List.mem x liste1;;

let vide = ('~', '~');;

(* écrit le phonème selon l'alphabet phonetique internationnal *)
let rec api  = function
  | p when p = vide -> ""
  | c, '_' -> "("^ ( api (c,' ') ) ^")"

  | 'a', ' ' -> "a"  | 'e', ' ' -> "e"  | 'i', ' ' -> "i" | 'o', ' ' -> "o" 
  | 'u', ' ' -> "u"  | 'y', ' ' -> "y"  | 'q', ' ' -> "ə"
  | 'x', ' ' -> "ø"

  | 'b', ' ' -> "b"  | 'd', ' ' -> "d"  | 'f', ' ' -> "f"  | 'g', ' ' -> "g"
  | 'k', ' ' -> "k"  | 'l', ' ' -> "l"  | 'm', ' ' -> "m"  | 'n', ' ' -> "n"
  | 'p', ' ' -> "p"  | 'r', ' ' -> "ʁ"  | 's', ' ' -> "s"  | 't', ' ' -> "t"
  | 'v', ' ' -> "v"  | 'z', ' ' -> "z"

  | 'h', ' ' -> "ɥ"  | 'w', ' ' -> "w"  | 'j', ' ' -> "j"

  | 'n', '~' -> "ɲ"  | 'g', '~' -> "ŋ"  | 'a', '~' -> "ɑ̃"  | 'e', '~' -> "ɛ̃"
  | 'o', '~' -> "ɔ̃"

  | 'a', '^' -> "ɑ"  | 'e', '^' -> "ɛ"  | 'o', '^' -> "ɔ"  | 'x', '^' -> "œ"

  | 's', '^' -> "ʃ"  | 'z', '^' -> "ʒ"
  | c,t -> failwith (Printf.sprintf "Phonème inconnu : '%c%c'." c t)
