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

let stateMapFilter f map =
  StateMap.fold
    (fun s x m -> if f s x then StateMap.add s x m else m)
    map StateMap.empty;;

let list_of_map map = StateMap.fold (fun k a acc -> (k, a)::acc) map []

let swap v i j =
  let aux = v.(i) in
  v.(i) <- v.(j);
  v.(j) <- aux;;

let sqrt_int n = int_of_float (sqrt (float_of_int n));;

module ComplexBdd = functor (C : Contrainte.Constraint) ->
struct

  type t = ((State.t * C.metadata list) array * (int ref)) StateMap.t

  type trans = (State.t * C.metadata list) array * int * int

  type meta = C.metadata

  let frequency map x =
    try
      let (v, _) = StateMap.find x map in
      Array.length v
    with Not_found -> 0

  let sortByFrequency map =
    let my_compare (a, _) (b, _) =
      (Pervasives.compare (frequency map a) (frequency map b)) in
    StateMap.iter (fun _ (v, _) -> Array.sort my_compare v) map

  let build text = 
    let assoc = Bdd.build C.precompute text in
    let map = List.fold_left (fun map (key, l) -> (StateMap.add key ((Array.of_list  l), ref (-1)) map)) StateMap.empty assoc in
    sortByFrequency map; map

  let get map s =
    try
      let (v, current) = StateMap.find s map in
      incr current;
      v, 0, 0
    with 
      |Not_found -> raise Contrainte.ContrainteNonRespectee

  let choose ((succ, i, _) : trans) =
    let length = Array.length succ in
    if i = length || i = !Param.maxTries then raise Contrainte.ContrainteNonRespectee
    else
      let j = i + (Random.int (length - i)) in (* formule magique arbitraire..*)
      swap succ i j;                           (* machinerie à améliorer *)
      let res = succ.(i) in
      res, (succ, (i + 1), 0)

  let filter f map =
    let filterElem (array, _) =
      let v = Common.array_filter (fun (s, _) -> f s) array in
      (v, ref (Array.length v)) in
    stateMapFilter (fun s (array, _) -> f s && array <> [||])
      (StateMap.map filterElem map)
      
  let printStatesByArrity out ?(min=0) ?(max=max_int) (map : t) =
    let l = list_of_map map in
    let withlength = List.map (fun (k, (v, _)) -> (k, Array.length v)) l in
    let to_print =
      (List.sort (fun (_, i) (_, j) -> compare i j)
	 (List.filter (fun (_, l) -> min <= l && l <= max ) withlength)) in
    List.iter (fun (s, i) -> Printf.fprintf out "%s : %i\n" (State.sprint s) i) to_print

  let printArrow (state, metalist) =
    (Printf.sprintf "%s : \n\t\t%s"
      (State.sprint state)
      (String.concat "\n\t\t" (List.map C.print_metadata metalist)))

  let printMapElem k (v, _) =
    (Printf.sprintf "%s\n\t%s\n"
      (State.sprint k)
      (String.concat "\n\t" (List.map printArrow (Array.to_list v))))

  let printAll out map =
    StateMap.iter (fun k v -> Printf.fprintf out "%s" (printMapElem k v)) map

  let possibleTags map word =
    let res = ref [] in
    Array.iter (fun tag -> if StateMap.mem (State.make (word, tag)) map then res := tag :: !res) Tag.all;
    let freq tag = frequency map (State.make (word, tag)) in
    let myCompare t1 t2 = compare (freq t2) (freq t1) in
    List.sort myCompare !res

end
