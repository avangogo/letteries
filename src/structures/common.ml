(** Some basic functions that could have been in a standard library *)

(** transform a string into a list of char *)
let list_of_string s =
  let res = ref [] in
  for i = String.length s - 1 downto 0 do
    res := s.[i] :: !res
  done;
  !res

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
