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
let normalize input output =
  let inChannel = open_in input
  and outChannel = open_out output in
  Normalize.token outChannel (Lexing.from_channel inChannel);
  close_out outChannel;
  close_in inChannel;;

let treeTagger input output =
  ignore (Unix.system (
    Printf.sprintf "./treetagger/cmd/tree-tagger-french %s > %s" input output
  ));;

let readTreeTaggerOutput input =
  let inChannel = open_in input in
  let result = ReadTreeTagger.token [] (Lexing.from_channel inChannel) in
  close_in inChannel;
  result;;

let printTreeTaggerOutput treetagger output =
  let outChannel = open_out output in
  List.iter (fun (w, tag) -> Printf.fprintf outChannel "%s\t%s\n" w (Tag.string_of_tag tag)) treetagger;
  close_out outChannel;;

let timeTag = ("treeTag", ref 0., ref 0.);;
let timeNormalize = ("Normalize", ref 0., ref 0.);;
let timeReadTagger = ("ReadTreeTagger", ref 0., ref 0.);;
let timeLearn = ("Learn", ref 0., ref 0.);;

let p = Printf.printf "%s\n"

let start (_, _, sys) =
  sys := Unix.time ();;
let stop (s, t, sys) =
  t := !t +. (Unix.time () -. !sys);
  Printf.printf "Total time %s : %f\n" s !t;
  flush stdout;;
let print_systime =
  let t0 = Unix.time () in function () ->
  Printf.printf "Sys.time: %f\n" (Sys.time ());
  Printf.printf "Unix.time: %f\n" (Unix.time () -. t0);
  flush stdout;;  

let parse filename =
  normalize filename "tmp/normalize";
  treeTagger "tmp/normalize" "tmp/withtag";
  let text = readTreeTaggerOutput "tmp/withtag" in
  text;;

let getFiles rep =
  let files = List.sort compare (Array.to_list (Sys.readdir rep)) in
  List.map ((^) rep) files;;


(* met sous la forme avec "ponctuation "*)
let sortStates l =
  let rec aux acc punct = function
    |((_, tag) as t)::q ->
      if Grammaire.isRelevant tag
      then aux ((t, punct)::acc) [] q
      else aux acc (t::punct) q
    |[] -> acc in
  aux [] [] (List.rev l);;

(* the last action to do with treeTagger output*)
let return name text =
  Grammaire.learn (List.map snd text);
  name, sortStates text;;

(* censé faire la meme chose que la fonction du meme nom du module lecture *)
(*let recupere_textes dossier_corpus (dossiers : string list) =*)
  (* Liste desfic hiers *)
(*  let fichiers = List.concat (List.map getFiles (List.map ((^) dossier_corpus) dossiers)) in*)
  (* Récupere les textes *)(*
  let lireFichier nom =
    let texte = parse nom in
    Grammaire.learn (List.map snd texte); *)(* Donne les listes de tag au module grammaire *)
 (*   nom, (sortStates texte) in
  List.map lireFichier fichiers;;*)

let recupere_pretraite dossier =
  let fichiers = getFiles dossier in
  let lireFichier nom =
    Printf.printf "Recuperation : %s.\n" nom; flush stdout; 
    let texte = readTreeTaggerOutput nom in    
    return nom texte in
  List.map lireFichier fichiers;;

(*normalize "res/faune" "res/tmp";;
treeTagger "res/tmp" "res/tmp2";;
print_string "toto";;
let x =readTreeTaggerOutput "res/tmp2" in
printTreeTaggerOutput x "res/tmp3";;*)
