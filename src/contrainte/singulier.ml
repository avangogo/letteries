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

(* contrainte to prevent from using a word twice*)
open Word
module StringSet = Set.Make (String)

(* -- begin of the constraint-- *)
type metadata = string option
type state = StringSet.t

let precompute w =
  if w.relevant then Some w.lemma else None
    
let final _ = true

let step map = function
  |None -> map
  |Some s ->
    if StringSet.mem s map then raise Contrainte.ContrainteNonRespectee
    else StringSet.add s map

let init_state () = StringSet.empty

  (*pretty-printing*)
let name = "Singulier";;
let print_state _ = "<StringSet>";;
let print_metadata = function
  |None -> "_"
  |Some s -> s
