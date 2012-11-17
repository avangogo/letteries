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

type phoneme = Phoneme.phoneme
type automate =
    ((char * int array) list * char list array * int) * (*premier automate*)
      ((char * int array) list * char list array * int) * (*second automate*)
      (char * Phoneme.phoneme list array array) list (*fonction de résolution*)
type rime = phoneme list
type muet = phoneme option * phoneme option

let phoneme_vide = Phoneme.vide
let rime_vide = []
let muet_vide = (None,None)
let make_automate fichier = Outils.construit_machine fichier
let of_string = Outils.traduit_phrase
let liaison = Outils.liaison
let fin_muette = Outils.fin_muette
let concat = Outils.concat
let rime = Outils.rime
let nbre_voyelles = Outils.nbre_voyelles
let string_of_phoneme = Affichage.so_phoneme
let string_of_rime r = String.concat "" (List.map Affichage.so_phoneme r)
let string_of_muet (p1, p2) =
  let aux = function
    |Some x -> Affichage.so_phoneme x
    |None -> "'_'" in
  Printf.sprintf "%s%s" (aux p1) (aux p2)
let print l = List.iter Affichage.p_phoneme l
