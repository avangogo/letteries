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

(***********création et manipulation des listes d'association****************)
let compare_assoc x y = compare (fst x) (fst y);;

(*rassemble les éléments associés à une clef*)
(*fusionne deux listes d'assoiciations triées*)
let rec fusionne_triee = function (*complexité améliorable*)
  |(a,la)::(b,lb)::q  when a=b -> fusionne_triee ((a,(lb@la))::q)
  |t::q                        -> t::(fusionne_triee q)
  |[]                          -> [];;

(*prend en entrees des aretes (clef,valeur)*)
let fusionne_assoc l =
  let aux (c,v) = (c,[v]) in
  let l1 = List.map aux l in
  let l2 = List.sort compare_assoc l1 in
  fusionne_triee l2;;

let liste_succession precompute texte =
  let rec aux = function
    |(ma, pa)::(mb,pb)::q ->
      let metadata = (List.rev ((precompute ma)::(List.map precompute pa))) in
      (Word.state_of_word mb, (Word.state_of_word ma, metadata))::
	(aux ((mb, pb)::q))
    |_            -> []
  in aux texte;;
  
let build precompute textes =
  fusionne_assoc (List.concat (List.map (liste_succession precompute) textes));;
 
(***)
module type Bdd = functor (C : Contrainte.Constraint) ->
sig
  type t
  type trans
  type meta = C.metadata
  val build : (Word.word * Word.word list) list list -> t
  val get : t -> State.t -> trans
  val choose : trans -> (State.t * C.metadata list) * trans
end

  
(*****************************************)
module ListBdd : Bdd = functor (C : Contrainte.Constraint) ->
struct
  type trans = (State.t * C.metadata list) list
  type t = (State.t * trans) list
  type meta = C.metadata
  let build textes = build C.precompute textes
  let get bdd x =
    try
      List.assoc x bdd
    with
      |Not_found -> raise Contrainte.ContrainteNonRespectee
  let choose = function
    |t::q -> t,q
    |[]   -> raise Contrainte.ContrainteNonRespectee
end
