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

exception Erreur_externe of string;;
let ps x = print_string x;;
let p x = print_string x; print_newline ();;

(*************parser le fichier "texte"**************)
let parse texte =
  ps "Lecture : "; p texte;
  let entree = open_in texte in
  let res0 = Lexing.from_channel entree
  in
  let res1 =
    try
      Parser_l.main Lexer_l.token res0 
    with
      |Lexer_l.Caractere_inconnu (c, p) ->
	failwith (Printf.sprintf "Erreur en lisant %s, ligne %i caractère %i\nCaractère inconnu : ‘%c’ ‘%i’."
		    p.Lexing.pos_fname p.Lexing.pos_lnum p.Lexing.pos_bol c (int_of_char c))
  in
(*  p "fin lecture";*)
  close_in entree;
  res1;; (*c'est une liste de textes*)

let recupere_name rep =
  let files = List.sort compare (Array.to_list (Sys.readdir rep)) in
  List.map ((^) rep) files;;

(***récupérer les différents textes, les formater et les assembler****)
let recupere_textes dossier_corpus (dossiers : string list) =
  let fichiers = List.concat (List.map recupere_name (List.map ((^) dossier_corpus) dossiers)) in
  let liste0 = List.map (fun f -> List.map (fun text -> f, text) (parse f)) fichiers in
  let liste1 = List.concat liste0 in
  liste1;;

