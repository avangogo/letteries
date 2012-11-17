(* algorithme général *)
let engendre get choose go x0 =
  let rec engendre0 x =
    explore x (get x)
  and explore x next0 =
    let arrow, next1 = choose next0 in
    try
      engendre0 (go x arrow)
    with
      |Contrainte.ContrainteNonRespectee -> explore x next1
  in
  engendre0 x0;;

(* 
state = position dans l'exploration
trans (transisitons) = ensemble de position à explorer depuis une position
arrow = action à faire lors d'un changement d'état

get : state -> trans
choose : trans -> arrow * trans
go : state -> arrow -> state
*)

(*type state = string * C.state
  type trans = Markov.trans
  type arrow = string * C.metadata list*)



module Engendre (C:Contrainte.Constraint) (Markov :
sig
  type t
  type trans
  val get : t -> State.t -> trans
  val choose : trans -> (State.t * C.metadata list) * trans
end) =
struct
  let write (markov : Markov.t) last_word init =
    Printf.printf "Constraint name : %s.\n" C.name;
    let get (s, _) = Markov.get markov s
    and choose = Markov.choose
    and go (s1, m) (s2, l) = (s2, (List.fold_left C.step m l)) in
    try
      engendre get choose go (last_word, init)
    with
      |Record.End l -> l
end
