(* ** *)
type 'a dictionnary = ('a, int) Hashtbl.t * 'a array

let remove_doubles l =
  let rec aux acc = function
    | a :: b :: q ->
      if a = b then aux acc (a :: q)
      else aux (a :: acc) (b :: q)
    | [ a ] -> a :: acc 
    | [] -> acc in
  aux [] (List.sort compare l);;

let makeToInt l0 =
  let l = remove_doubles l0 in
  let n = List.length l in
  let tble = Hashtbl.create n
  and invtble = Array.make n (List.hd l) in
  let i = ref (-1) in
  List.iter (fun x -> incr i; invtble.(!i) <- x; Hashtbl.add tble x !i) l;
  tble, invtble;;

let to_int (tble, _) x = Hashtbl.find tble x;;

let of_int (_, tab) i = tab.(i);;

let elem (_, tab) = Array.to_list tab;;

let size (_, t) = Array.length t;;


(* ********* ******** *)
type 'b tree =
  | N of 'b list * 'b tree array
  | L of 'b list;;

type ('a, 'b) truc = 'a dictionnary * 'b tree;;

(* all_equal c l renvoie true si pour tout a et b dans l, (c a) = (c b) *)
let all_equal carac = function
  | [] -> true
  | a::q -> let ca = carac a in List.for_all (fun b -> ca = carac b) q;;

let safe_tl = function
  | _ :: q -> q
  | [] -> [];;

let makeTree carac sigma rules =
  let m = List.length sigma in
  let sigmatbl = makeToInt sigma in
  let rec makeAssoc list =
    N (List.map snd list, Array.init m (fun i -> select i list))
  and select alpha list0 =
    let list1 = List.filter (fun (l, _) -> (l = [])||(List.hd l = alpha)) list0 in
    let list2 = List.map (fun (l, r) -> (safe_tl l, r)) list1 in
    if all_equal fst list2 then L (List.map snd list2)
    else makeAssoc list2 in
  sigmatbl, makeAssoc (List.map (fun r -> List.map (to_int sigmatbl) (carac r), r) rules);;

(*  *)
let getTree v (tble, tree) = 
  let rec aux w = function
  | L res -> res
  | N (res, assoc) ->
    match w with
      |t::q -> aux q assoc.(to_int tble t)
      |[] -> res in
  aux v tree;;

(* **** *)
let print p (_, t) =
  Array.iteri (fun i e -> Printf.printf "%d : %s\n" i (p e)) t
