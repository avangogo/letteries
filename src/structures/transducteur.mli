type state

type ('a, 'b) transducer

type ('a, 'b) localRule = Arrow of 'a list * 'a list * 'a list * 'b list

type ('a, 'b) localGrammar = ('a, 'b) localRule list

val make :
  'a list -> ('a, 'b) localGrammar -> ('a, 'b) transducer

val transduce :
  ('a, 'b) transducer -> 'a list -> 'b list
