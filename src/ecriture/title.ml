open Word

let rec list_nth i = function
  | t :: q -> if i = 0 then t else list_nth (i - 1) q
  | [] -> failwith "list_nth"

let random_list l =
  list_nth (Random.int (List.length l)) l;;


let title l =
  let adjs = List.filter (fun w -> w.tag = Tag.ADJ) l
  and noms = List.filter (fun w -> w.tag = Tag.NOM) l in
  let adj = random_list adjs
  and nom = random_list noms in
  Printf.sprintf "%s %s" (String.capitalize adj.lemma) nom.lemma;;
