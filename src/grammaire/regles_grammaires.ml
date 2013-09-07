(*
type detTag = ART | Pos
type proTag = DEM | IND | PER | POS | REL
type prpTag = Det
type punTag = Cit
type verTag =
    Cond | Futu | Impe | Impf | Infi | Pper | Ppre
  | Pres | Simp | Subi | Subp
type tag =
    ABR
  | ADJ
  | ADV
  | DET of detTag
  | INT
  | KON
  | NAM
  | NOM
  | NUM
  | PRO of proTag option
  | PRP of prpTag option
  | PUN of punTag option
  | SENT
  | SYM
  | VER of verTag
*)

open Finiteautomaton
open Tag

let get_grammar () =
  let inChannel = open_in !Param.grammarrules_file in
  let res = Parser_g.main (Lexer_g.token) (Lexing.from_channel inChannel) in
  close_in inChannel;
  res;;

let nd_comp a = nondeterministic_of_t (complement (determinize a));;
let nd_inter a b = nondeterministic_of_t (intersection (determinize a) (determinize b));;

let f x = minimize (determinize x)

let min_length sigma min =
  let sent = nd_letter sigma (int_of_tag SENT) in
  let all = nd_comp (nd_empty sigma) in
  let no_sent = nd_comp (nd_concat (nd_concat all sent) all) in
  let oneletter = nd_inter (nd_comp (nd_epsilon sigma)) no_sent in
  let a = ref sent in
  for i = 1 to min do
    a := nd_concat oneletter !a
  done;
  nd_star !a;;
  

let x =
  let a = get_grammar () in
  let b = min_length (nd_sigma a) !Param.minSentenceLength in
  nd_inter a b;;

let auto = minimize (determinize (nd_rev (x)));;
