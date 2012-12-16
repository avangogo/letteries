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

type phoneme

val of_string : string -> phoneme
val to_string : phoneme -> string

(* liste des phonemes non-muets *)
val liste : phoneme list
(* dit si l'entrée est un phoneme non-muet *)
val est_valide : phoneme -> bool

(* liste des phonemes muets *)
val liste0 : phoneme list
val est_valide0 : phoneme -> bool

(* liste de tous les phonèmes *)
val liste1 : phoneme list
val est_valide1 : phoneme -> bool

val voyelle : phoneme -> bool
val consonne : phoneme -> bool
val muet : phoneme -> bool

(* transforme un muet en non-muet *)
val exprime : phoneme -> phoneme

(* créé un phoneme muet *)
val of_string0 : string -> phoneme

val vide : phoneme

val api : phoneme -> string
