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

let corpus_subdirs = !Param.corpus_subdirs
let corpus_dir = !Param.corpus_dir
let computed_dir = !Param.computed_dir
let dir_tmp = !Param.tmp_dir

let new_id =
  let i = ref 0 in
  function () -> incr i; "text_"^(string_of_int !i);;

(* let normalized_basename name =
  let normalize = Common.string_map (function ' ' -> '_' | c -> c) in
  normalize (Filename.basename name)*)

(* Un moyen simple mais sale de récupérer le nom et le recueil du poème *)
let source_of_filename name =
  let poemName = UseCamomile.latin0_of_utf8 (Filename.basename name) in
  let bookName = UseCamomile.latin0_of_utf8 (Filename.basename (Filename.dirname name)) in
  Word.make_source ~text:poemName ~book:bookName ();;


let precompute () =
  let fichiers = List.concat
    ( List.map Lecture.getFiles
	( List.map ( (^) !Param.corpus_dir ) !Param.corpus_subdirs ) ) in
  let lireFichier name =
    Print.p (Printf.sprintf "Lecture de %s" name);
    let source_text = Word.sprint_source (source_of_filename name) in
    let tmp = !Param.tmp_dir ^ "normalize" in
    Lecture.normalize ~header:source_text name tmp;
    Lecture.treeTagger tmp ( !Param.computed_dir ^ ( new_id () ) ) in
(*    Lecture.treeTagger tmp ( !Param.computed_dir ^ ( normalized_basename name ) ) in*) (* FIXME : faire quelque chose si un fichier du même nom existe *)
  List.iter lireFichier fichiers;;

let main () =
  ignore
    (Unix.system
       (Printf.sprintf "rm %s*" !Param.computed_dir));
  precompute ();;
