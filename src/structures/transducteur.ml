open Minidata

type state = int

(* initial state, transitions *)
type ('a, 'b) transducer = state * ('a * (state * 'b list)) list array

type ('a, 'b) localRule =
    Arrow of 'a list * 'a list * 'a list * 'b list

type ('a, 'b) localGrammar = ('a, 'b) localRule list

let size (_, t) = Array.length t

let left_context = function
  | Arrow( c, _, _, _ ) -> c

let rec prefix pre w =
  match pre with
    |[] -> true
    |t1::q1 ->
      match w with
	|[] -> false
	|t2::q2 -> (t1=t2)&&(prefix q1 q2);;

let rec strict_prefix pre w =
  match w with
    |[] -> false
    |t2::q2 ->
      match pre with
	|[] -> true
	|t1::q1 -> (t1=t2)&&(strict_prefix q1 q2);;

type positionRelative =
  | GreaterOrEqual
  | Lesser
  | Uncomparable;;


let rec compare_prefix a b =
  match a with
    |[] -> GreaterOrEqual (* a est un préfixe de b *)
    |t1::q1 ->
      match b with
	|[] -> Lesser (* b est un préfixe strict de a *)
	|t2::q2 -> if t1=t2
	  then compare_prefix q1 q2
	  else Uncomparable;;
  
let rec prefix_sym v w =
  match v,w with
    |[], _ -> true
    |_, [] -> true
    |t2::q2, t1::q1 when t1=t2 -> prefix_sym q1 q2
    |_ -> false;;

let suffix s w =
  prefix (List.rev s) (List.rev w)

let rec inv p w =
  match p, w with
    |[], _ -> w
    |t1::q1, t2::q2 when t1 = t2 -> inv q1 q2
    |_ -> failwith "inv";;

(* dit si deux règles peuvent être simultanément compatibles *)
let is_compatible r1 r2 =
  let aux = function Arrow( a, w, b, _) -> a, (List.rev b)@(List.rev w) in
  let (v1, u1) = aux r1
  and (v2, u2) = aux r2 in
  (prefix_sym v1 v2) && (prefix_sym u1 u2);;

(* dit si "r1 s'applique => r2 s'applique" *)
let is_more_general r1 r2 =
  let aux = function Arrow( a, w, b, _) -> a, (w@b) in
  let (v1, u1) = aux r1
  and (v2, u2) = aux r2 in
  (suffix v1 v2) && (prefix u1 u2);;


let word_and_suffixe = function
  | Arrow( _, w, s, _ ) -> w@s

let fst_letter = function
  | c :: _ -> c
  | _ -> Obj.magic 0;;

let index_rules = function
  |Arrow(_, w, _, _) -> fst_letter w;;

(*let crade = ref (Obj.magic 0);;
let ps = print_string
let pc = print_char
let pn = print_newline
let p l = List.iter pc l; pn ()
let scharlist (l : char list) = String.concat "" (List.map (String.make 1) l)
let sstate (s, w) = Printf.sprintf "[%s]%s " (scharlist (Automate.traduit_etat !crade s)) (scharlist w);;
let pstate s = Printf.printf "%s\n" (sstate s);;
let strans (c, (s, l)) = Printf.sprintf "  %c -- %s --> %s" c (scharlist l) (sstate s);;
let ptrans t = Printf.printf "%s\n" (strans t)
let p_ (s, l) = pstate s; pn (); List.iter ptrans l
let pdelta l = List.iter (fun (s, succ) -> pstate s; List.iter p_ succ) l
let pt (c, (s, l)) = Printf.printf "  %c -- %s --> %i\n" c (scharlist l) s;;
let pauto (n, init, trans) =
  Printf.printf "n : %i\ninit : %i\n" n init;
  Array.iteri (fun i t -> Printf.printf "%i\n" i; List.iter pt t) trans;;
let pi i = print_int i; print_newline ();;
let pregle = function Arrow( u, v, w, _ ) -> Printf.printf "[%s]%s[%s] -> ...\n" (scharlist u) (scharlist v) (scharlist w);;*)

let rec simplify = function
  |t :: q -> t::(simplify (List.filter (fun x -> not (is_more_general t x)) q))
  |[] -> [];;


let i = ref 0;;
let j = ref 0;;

(*  *)
let rec match_rule auto statetble out (pre_id, v) rules0 =
  let rules = getTree v rules0 in
  (* crade := Obj.magic auto;*)
  incr i; (*Printf.printf "%d  " (List.length rules);
  pstate (Obj.magic (pre_id, v));*)
  let pre = Automate.traduit_etat auto pre_id in
  let pre_inv = List.rev pre in
  let default = (pre_id, v), out in
  let rec aux =
    function
      | Arrow(c1, w, c2, o ) :: q ->	
	if prefix (List.rev c1) pre_inv (* prefix c1 pre_inv *) 
	then
	  match compare_prefix (w@c2) v with
	    | Uncomparable -> aux q
	    | Lesser -> default
	    | GreaterOrEqual ->
	      let new_v = inv w v in
	      let new_pre_id = List.fold_left (Automate.transition auto) pre_id w in
	      let new_state = new_pre_id, new_v in
	      let new_out = out@o in
	      if Hashtbl.mem statetble new_state
	      then (new_state, new_out)
	      else match_rule auto statetble new_out (new_pre_id, new_v) rules0
	else aux q
    | [] -> default in
  if v = [] then default else
    (incr j;
    aux rules);;

let start () = Sys.time ();;
let stop s t = Printf.printf "temps %s: %f\n" s (Sys.time () -. t);;

(* prend en entrée une liste de regles et construit le transducteur associé *)
let make sigma0 rules0 =

  let m = List.length sigma0 in
  let sigma = makeToInt sigma0 in

  (* let a = start () in *)

  let rules = makeTree word_and_suffixe (elem sigma) rules0 in
  
  (* stop "makeTree" a;*)
  
  let prefix = Automate.make (elem sigma) (List.map left_context rules0) in
  (* Printf.printf "Nombre de prefixes : %i\n" (Automate.length prefix);
  Printf.printf "Regles avec [] : %i\n" (List.length rules0);
  Printf.printf "Regles avec [] : %i\n" (List.length (List.filter (fun x -> left_context x = []) rules0));*)
  let init = Automate.initial, [] in
  let states = Hashtbl.create (m*m)
  and delta = ref []
  and to_do = Stack.create () in
  let new_id =
    let i = ref (-1) in
    function () -> incr i; !i in
  let add_state id s =
    Hashtbl.add states s id;
    Stack.push (s, id) to_do in
  let get_id s =
    try Hashtbl.find states s with
      | Not_found -> let id = new_id () in
		     add_state id s; id
  in
  add_state (new_id ()) init;
  let succ (pre, w) alpha = match_rule prefix states [] (pre, w@[alpha]) rules in
  let calc_succ state alpha =
    let (s, o) = succ state alpha in
    let s_id = get_id s in
    (alpha, (s_id, o))
  in

  (*let b = start () in*)

  while not (Stack.is_empty to_do) do
    let state, id_state = Stack.pop to_do in
    delta := (id_state, (List.map (calc_succ state) (elem sigma)))::(!delta)
  done;

  (*stop "boucle" b;*)

  (* normalisation *)
  let n = new_id () in
  (* Printf.printf "nombre d'appels : %d\n" !i;
  Printf.printf "nombre d'appels non vides : %d\n" !j;
  Printf.printf "nombre d'états : %d\n" n;*)
  let new_delta = Array.make n [] in
  List.iter (fun (i, t) -> new_delta.(i) <- t) !delta;
  (get_id init, new_delta);;

(* Minimisation *)



(* Exécution *)
let transduce (init, trans) w =
  let step (s, out) a =
    let (new_s, o) = List.assoc a trans.(s) in (new_s, out@o)
  in
  let (_, res) = List.fold_left step (init, []) w in
  res;;



(* ===================== *)
let r =
  [
    Arrow( ['b'], ['a';'a'], ['a'], ['2']);
    Arrow( [], ['a'], [], ['A']);
    Arrow( [], ['b'], [], ['B']);
    Arrow( [], ['f'], [], ['E';'N';'D'])
  ];;

let sigma = ['a'; 'b'; 'f'];;


