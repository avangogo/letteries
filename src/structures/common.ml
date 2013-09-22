(* Some basic functions that could have been in a standard library *)

(* transform a string into a list of char *)
let list_of_string s =
  let res = ref [] in
  for i = String.length s - 1 downto 0 do
    res := s.[i] :: !res
  done;
  !res

(* array_filter f t return an array containing the elements x of t
that satisfy f x, the order is preserved *)
let array_filter f v =
  Array.of_list (List.filter f (Array.to_list v))
 
