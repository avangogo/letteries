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

open Regles;;
open Transducteur;;

type automate = (char, Phoneme.phoneme) transducer *
    (string, Phoneme.phoneme list) Hashtbl.t (* table des exceptions *)

exception Cassansregle of (char list)*(char list);;
exception Toto of char;;

let size (t, _) = Transducteur.size t

let table_des_exceptions assoc =
  let res = Hashtbl.create (List.length assoc) in
  List.iter (fun (mot, phon) -> Hashtbl.add res mot phon) assoc;
  res;;

let precalcul (regles, exceptions) =
  let automate = Transducteur.make (caractere_fin::Regles.alphabet) regles
  and tableexceptions = table_des_exceptions exceptions in
  (automate, tableexceptions)

let charlist_of_string s =
  let n = String.length s in
  let res = ref [caractere_fin] in
  for i = n-1 downto 0 do
    res := s.[i] :: !res
  done;
  !res;;

let traduit (auto, exceptions) mot =
  try
    Hashtbl.find exceptions mot
  with Not_found ->
    transduce auto (charlist_of_string mot) (* enlever la sérialisation? *)
