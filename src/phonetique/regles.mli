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

type lettre0 =
  | Var0 of char
  | Lettre0 of char
  | Fin0

type regle0 =
    Fleche0 of lettre0 list * lettre0 list * lettre0 list *
	Phoneme.phoneme list

type lettre1 =
  | Lettre1 of char
  | Fin1

type regle1 =
    Fleche1 of lettre1 list * lettre1 * lettre1 list * Phoneme.phoneme list

type regle2 = Fleche2 of char list * char list * Phoneme.phoneme list

exception ErreurDeRegle of string
val alphabet : char list
val caractere_fin : char

val construit_1 :
  (char * lettre0 list) list * regle0 list * (string * Phoneme.phoneme list) list ->
  regle1 list * (string * Phoneme.phoneme list) list
val construit_2 : regle1 list * 'a -> (char * regle2 list) list * 'a
