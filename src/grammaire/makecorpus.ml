(* 
    Lettreries is a random poem generator.
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
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

let dirs = ["Poésies(Mallarmé,1914)/";
	    "Les fleurs du mal (1868)/";
	    "Rimbaud, Poésies/"];;

let corpus_dir = "data/corpus/";;
let dir_computed = "data/computed/";;


let new_id =
  let i = ref 0 in
  function () -> incr i; "text_"^(string_of_int !i);;

let precompute dossier_corpus (dossiers : string list) =
  let fichiers = List.concat
    (List.map Lecture_gram.getFiles
       (List.map ((^) dossier_corpus) dossiers)) in
  let lireFichier name =
    Lecture_gram.normalize name "tmp/normalize";
    Lecture_gram.treeTagger "tmp/normalize" (dir_computed^(new_id ())) in
  List.iter lireFichier fichiers;;

precompute corpus_dir dirs;;
