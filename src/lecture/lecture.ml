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
open Word

(* rewrite input on output with only authorized characters *)
let normalize input output =
  let inChannel = open_in input
  and outChannel = open_out output in
  Normalize.token outChannel (Lexing.from_channel inChannel);
  close_out outChannel;
  close_in inChannel;;

let treeTagger input output =
  ignore (Unix.system (
    Printf.sprintf "%s %s > %s" !Param.treetagger_script input output
  ));;

(* read the file outputed by TreeTagger *)
let readTreeTaggerOutput input =
  let inChannel = open_in input in
  let result = ReadTreeTagger.token [] (Lexing.from_channel inChannel) in
  close_in inChannel;
  result;;

(* read any normalized file and output something of the same type of readTreeTaggerOutput *)
let readAndTag input =
  let inChannel = open_in input in
  let result = ReadAndTag.token [] (Lexing.from_channel inChannel) in
  close_in inChannel;
  result;;

let printTreeTaggerOutput treetagger output =
  let outChannel = open_out output in
  List.iter (fun (w, tag) -> Printf.fprintf outChannel "%s\t%s\n" w (Tag.string_of_tag tag)) treetagger;
  close_out outChannel;;

(*let parse filename =
  let tmp_normalize = !Param.tmp_dir ^ "normalize"
  and tmp_withtag = !Param.tmp_dir ^ "withtag" in
  normalize filename tmp_normalize;
  treeTagger tmp_normalize tmp_withtag;
  let text = readTreeTaggerOutput tmp_withtag in
  text;;*)

let getFiles rep =
  let files = List.sort compare (Array.to_list (Sys.readdir rep)) in
  List.map ((^) rep) files;;


(* met sous la forme avec "ponctuation "*)
let punctuize l =
  let rec aux acc punct = function
    | t::q ->
      if t.relevant
      then aux ((t, punct)::acc) [] q
      else aux acc (t::punct) q
    |[] -> acc in
  aux [] [] (List.rev l);;

let makeWord file phon (w, tag, lemma) =
{
  word = w;
  tag = tag;
  (* micro-optimisation mémoire*)
  lemma = if w = lemma then w else lemma;
  file = file;
  phonetic = phon w;
  relevant = Grammaire.isRelevant tag
}


(* the last action to do with treeTagger output*)
let return phon name text =
  (* Grammaire.learn (List.map snd text);*)
  let wordList = List.map (makeWord name phon) text in
  punctuize wordList;;

let getComputed phon dossiers =
  let fichiers = List.concat (List.map getFiles dossiers) in
  let lireFichier nom =
    (*Print.verbose (Printf.sprintf "Recuperation : %s." nom);*)
    let texte = readTreeTaggerOutput nom in
    return phon nom texte in
  List.map lireFichier fichiers;;

let getRaw phon dossiers =
  let tmp_normalize = !Param.tmp_dir ^ "normalize" in
  let fichiers = List.concat (List.map getFiles dossiers) in
  let lireFichier nom =
    Print.verbose (Printf.sprintf "Lecture : %s." nom);
    normalize nom tmp_normalize;
    let texte = readAndTag tmp_normalize in
    return phon nom texte in
  List.map lireFichier fichiers;;

