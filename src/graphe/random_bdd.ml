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

module StateMap = Map.Make (State);;


let swap v i j =
  let aux = v.(i) in
  v.(i) <- v.(j);
  v.(j) <- aux;;

module RandomBdd : Bdd.Bdd = functor (C : Contrainte.Constraint) ->
struct
  type t = ((State.t * C.metadata list) array) StateMap.t
  type trans = (State.t * C.metadata list) array * int
  type meta = C.metadata
  let build text = 
    let assoc = Bdd.build C.precompute text in
    List.fold_left (fun map (key, l) -> (StateMap.add key (Array.of_list  l) map)) StateMap.empty assoc
  let get map s =
    try
      let v = StateMap.find s map in
      v, (Array.length v)
    with 
      |Not_found -> raise Contrainte.ContrainteNonRespectee
  let choose (succ, i) = (* petit effet de bord *)
    if i = 0 || (Array.length succ) - i > !Param.maxTries then raise Contrainte.ContrainteNonRespectee
    else
      let j = Random.int i in
      let res = succ.(j) in
      swap succ (i - 1) j;
      res, (succ, (i - 1))
end
