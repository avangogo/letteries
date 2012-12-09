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

(* Implémentation des automates de suffixe/préfixe : *)
(* Etant donné un ensemble de mots L et son automate A, l'état de A après *)
(* avoir lu un mot w est le plus grand suffixe de w qui est préfixe d'un mot de L *)

exception Mauvais_argument of string

type etat = int

type 'a t

(* make sigma L renvoie l'automate associé à L dans l'alphabet sigma *)
val make :
  'a list -> 'a list list -> 'a t

val initial : etat

val transition : 'a t -> etat -> 'a -> etat

(* renvoie le suffixe auquel correspond un état *)
val traduit_etat : 'a t -> etat -> 'a list

val length : 'a t -> int
