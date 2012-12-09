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

val lis_regles :
  string ->
  (char * Regles.lettre0 list) list * Regles.regle0 list *
    (string * Phoneme.phoneme list) list
    
val construit_machine :
  string -> Traduction.automate

val liaison :
  Phoneme.phoneme option * Phoneme.phoneme option ->
  Phoneme.phoneme -> Phoneme.phoneme list

exception MauvaisArg of Phoneme.phoneme

val fin_muette :
  Phoneme.phoneme list -> Phoneme.phoneme option * Phoneme.phoneme option

(* Concatene en résolvant la liaison et l'expression des lettres muettes *)
val concat :
  Phoneme.phoneme list -> Phoneme.phoneme list -> Phoneme.phoneme list

val rime : Phoneme.phoneme list -> Phoneme.phoneme list

val nbre_voyelles : Phoneme.phoneme list -> int

val traduit_phrase :
  Traduction.automate -> string -> Phoneme.phoneme list

(* Ancien syteme de calcul du nombre de pieds (à supprimer?) *)
val traduit_mot :
  Phoneme.phoneme list ->
  Phoneme.phoneme * int * Phoneme.phoneme list *
  (Phoneme.phoneme option * Phoneme.phoneme option)

val tete0 : int * ('a option * 'b option) * 'c list

val avance :
  int * (Phoneme.phoneme option * Phoneme.phoneme option) * 'a ->
  Phoneme.phoneme * int * 'a *
  (Phoneme.phoneme option * Phoneme.phoneme option) ->
  int * (Phoneme.phoneme option * Phoneme.phoneme option) * 'a

val traduit_string :
  Traduction.automate ->
  string ->
  Phoneme.phoneme * int * Phoneme.phoneme list *
  (Phoneme.phoneme option * Phoneme.phoneme option)
