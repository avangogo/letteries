(** Some basic functions that could have been in a standard library *)

(** type somme générique à deux éléments *)
type ('a, 'b) sum =
  |L of 'a
  |R of 'b;;

(** list_iteri f l where l = a0::a1::..an computes f 0 a0; f 1 a1; ... ; f n an *)
let list_iteri f (*int -> 'a -> unit*) =
  let rec aux i  = function
    |t::q -> (f i t); aux (i+1) q
    |[]   -> () in
    aux 0;;

(** transform a string into a list of char *)
let list_of_string s =
  let res = ref [] in
  for i = String.length s - 1 downto 0 do
    res := s.[i] :: !res
  done;
  !res

(** transform a list of char into a string *)
let string_of_charlist l=
  let n = List.length l in
  let res = String.make n 't' in
    list_iteri (fun i c -> (res.[i] <- c)) l;
    res;;

(** array_filter f t return an array containing the elements x of t
that satisfy f x, the order is preserved *)
let array_filter f v =
  Array.of_list (List.filter f (Array.to_list v))
 
(** remove_duplicate l returns a list that contains all elements of l and each
 only once. The order may be changed *)

let remove_duplicate l =
  let rec aux acc = function
    | a :: b :: q ->
      if a = b then aux acc (a :: q)
      else aux (a :: acc) (b :: q)
    | [ a ] -> a :: acc 
    | [] -> acc in
  aux [] (List.sort compare l);;


let string_map f s =
  let res = String.copy s in
  for i = 0 to (String.length s) - 1 do
    res.[i] <- f s.[i]
  done;
  res
