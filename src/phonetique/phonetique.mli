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
type automate
type rime
type muet (* fin d'un mot dont l'expression dépend du mot suivant. *)

val phoneme_vide:
  phoneme
val rime_vide:
  rime
val muet_vide:
  muet
val make_automate :
  string -> automate
val of_string :
  automate -> string -> phoneme list
val liaison :
  muet -> phoneme -> phoneme list
val fin_muette :
  phoneme list ->  muet
val concat :
  phoneme list -> phoneme list -> phoneme list
val rime :
  phoneme list -> rime
val nbre_voyelles :
  phoneme list -> int
val string_of_phoneme :
  phoneme -> string
val string_of_rime :
  rime -> string
val string_of_muet :
  muet -> string
val print :
  phoneme list -> unit
val prettyprint_api_utf8 :
  phoneme list -> unit
val size :
  automate -> int
