type 'a dictionnary
val makeToInt : 'a list -> 'a dictionnary
val to_int : 'a dictionnary -> 'a -> int
val of_int : 'a dictionnary -> int -> 'a
val size : 'a dictionnary -> int
val elem : 'a dictionnary -> 'a list

type ('a, 'b) truc

val makeTree :
  ('a -> 'b list) ->
  'b list -> 'a list -> ('b, 'a) truc
val getTree : 'a list -> ('a, 'b) truc -> 'b list
