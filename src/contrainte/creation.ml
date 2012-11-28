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

let new_number =
  let i = ref 0 in
  (fun () -> incr i; !i);;

let int_of_filename =
  let l = ref [] in
  let aux (name:string) =
    try
      List.assoc name !l
    with
      |Not_found ->
	let j = new_number () in
	l := (name, j)::(!l); j in
  aux;;

type metadata = int
type state = int
let precompute f b _ _ =
  if b then int_of_filename f else 0;;
let filter _ _ = true;;
let step j k  =
  if k=0 then j
  else if j=k then raise Contrainte.ContrainteNonRespectee
  else k;;
let init_state () = 0
(*pretty-printing*)
let name = "Creation";;
let print_state = string_of_int;;
let print_metadata = string_of_int;;
