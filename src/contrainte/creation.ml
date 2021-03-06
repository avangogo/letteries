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
open Word

let new_id =
  let i = ref 0 in
  (fun () -> incr i; !i);;

let int_of_filename =
  let l = ref [] in
  let aux (name:string) =
    try
      List.assoc name !l
    with
      |Not_found ->
	let j = new_id () in
	l := (name, j)::(!l); j in
  aux;;

(* ban sequences of three relevant words from the same poem *)
(* Warning: In particular, it forbids words that appear only once in the corpus*)
module Normal =
struct
  type metadata = int
  type state = int

  let precompute w =
    if w.relevant then int_of_filename w.file else 0

  let final _ = true

  let step j k  =
    if k=0 then j
    else if j=k then raise Contrainte.ContrainteNonRespectee
    else k

  let init_state () = 0

  (*pretty-printing*)
  let name = "Creation";;
  let print_state = string_of_int;;
  let print_metadata = string_of_int;;
end
  
(* weak version : ban sequences of four relevant words from the same poem *)
module Weak =
struct
  type metadata = int
  type state = int * int

  let precompute = Normal.precompute

  let final = Normal.final

  let step ( (a, b) as state ) c  =
    if c = 0 then state
    else if a = c && b = c then raise Contrainte.ContrainteNonRespectee
    else (b, c)

  let init_state () = 0, 0
  
  (*pretty-printing*)
  let name = "Creation (faible)"
  let print_state (a, b) = Printf.sprintf "(%i, %i)" a b
  let print_metadata = string_of_int
end

(* Remark : These classes could be generalized, I think it would be useless *)
