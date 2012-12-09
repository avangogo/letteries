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

val so_char : char -> string

val so_phoneme : Phoneme.phoneme -> string

val p_list : ('a -> unit) -> 'a list -> unit

val p_phoneme : Phoneme.phoneme -> unit

val p_lettre1 : Regles.lettre1 -> unit

val p_regle1 : Regles.regle1 -> unit

val p_lettre2 : char -> unit

val p_regle2 : Regles.regle2 -> unit

val p_assoc : ('a -> unit) -> ('b -> unit) -> ('a * 'b) list -> unit

val p_regle2s : (char * Regles.regle2 list) list -> unit

val p_option : ('a -> unit) -> 'a option -> unit

val p_tete :
  int * (Phoneme.phoneme option * Phoneme.phoneme option) *
  Phoneme.phoneme list -> unit
