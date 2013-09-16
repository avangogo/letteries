open Finiteautomaton
open Tag

let f x = minimize (determinize x)

let get_grammar rulesFile =
  let inChannel = open_in rulesFile in
  let res = Parser_g.main (Lexer_g.token) (Lexing.from_channel inChannel) in
  close_in inChannel;
  f (nd_rev res);;

let nd_comp a = nondeterministic_of_t (complement (determinize a));;
let nd_inter a b = nondeterministic_of_t (intersection (determinize a) (determinize b));;
let nd_minimize a = nondeterministic_of_t (minimize (determinize a))


let min_length sigma min =
  let sent = nd_letter sigma (int_of_tag SENT) in
  let all = nd_comp (nd_empty sigma) in
  let no_sent = nd_comp (nd_concat (nd_concat all sent) all) in
  let oneletter = nd_inter (nd_comp (nd_epsilon sigma)) no_sent in
  let a = ref sent in
  for i = 1 to min do
    a := nd_concat oneletter !a
  done;
  f (nd_rev (nd_star !a));;
 

let auto file =
  let a = get_grammar file in
  let b = min_length (sigma a) !Param.minSentenceLength in
  intersection a b;;

(* ancienne méthode *)
let old words =

  (* function to filter sentences *)
  let isAcceptable sentence =
    let l = List.length sentence in
    !Param.minSentenceLength <= l
    && l <= !Param.maxSentenceLength
    && not (List.mem INT sentence)
  in

  let confuse_letters auto list =
    List.fold_left Finiteautomaton.confuse_letters auto
      (List.map (List.map int_of_tag) list) in
  
  Print.verbose "Grammaire : liste des phrases";
  let words2 =
    (List.map List.rev
       (List.map
	  (List.map (Tag.int_of_tag)) 
	  (List.filter isAcceptable words))) in
  Print.verbose "Grammaire : premier automate";
  let nd_auto = Finiteautomaton.make_setstar_naive ~k:Tag.nbre words2 in
  Print.verbose "Grammaire : fusionne tags"; (*allègement des regles de grammaires*)
  let conf_auto  = confuse_letters nd_auto
    [[VER Futu; VER Impf; VER Pres; VER Simp];
     [VER Subi; VER Subp];
     [ADJ; VER Pper; VER Ppre];
     [PRO (Some DEM); DET ART; DET Pos; NUM]] in
  Print.verbose "Grammaire : determinisation";
  let det_auto = Finiteautomaton.determinize conf_auto in
  det_auto;;
