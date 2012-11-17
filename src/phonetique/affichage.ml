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

open Regles
open Phoneme

let ps = print_string;;
let pn = print_newline;;
let p x = ps x; pn();; 

let so_char c = let s = " " in s.[0] <-c ; s;;
let so_phoneme = function
  |C(a,b) -> Printf.sprintf "'%c%s'" a (if b = ' ' then "" else so_char b)

let p_list pe =
  pn ();
  List.iter (fun x -> pe x; pn ());;

let p_phoneme p =
  ps (so_phoneme p);;

let p_lettre1 = function
  |Lettre1 c -> ps (so_char c)
  |Fin1      -> ps "FIN";;


let p_regle1 = function
  |Fleche1(c1,x,c2,p) ->
     ps "[";
      List.iter p_lettre1 c1;
      ps "] ";
      p_lettre1 x;
      ps " [";
      List.iter p_lettre1 c2;
      ps "]";
      ps "    ->     ";
      List.iter p_phoneme p;;

let p_lettre2 c =
  ps (so_char c);;

let p_regle2 = function
  |Fleche2(c1,c2,p) -> 
    ps "[";
    List.iter p_lettre2 c1;
    ps "] ";
    ps " ";
    ps " [";
    List.iter p_lettre2 c2;
    ps "]";
    ps "    ->     ";
    List.iter p_phoneme p;;

let p_assoc p_clef p_elem =
  p_list (function (c,v) -> p_clef c; ps " : "; p_elem v; pn ());;

let p_regle2s l = p_assoc p_lettre2 (p_list p_regle2) l;;

let p_option p_elem = function
  |None -> ps "N"
  |Some x -> ps "S("; p_elem x; ps ")"

let p_tete (n,fin,rime) =
  print_int n; ps ", "; p_option p_phoneme (fst fin); ps " - ";
  p_option p_phoneme (snd fin); List.iter p_phoneme rime; p "";;
