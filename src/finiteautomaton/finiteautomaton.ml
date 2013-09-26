type state = int

type sigma = int

(* etat init, etats finaux, transitions*)
type t = { init : state; final : bool array; delta : state array array }

(* fonctions générales *)
let sigma a = Array.length a.delta.(0)
let size a = Array.length a.delta
let initState a = a.init
let deltaArray a = a.delta

let isFinal a i = a.final.(i)
let delta a i letter = a.delta.(i).(letter)

let empty = { init = 0; final = [|false|]; delta = [||] }


(* Affichage, debuggage *)
(*let p s = Printf.printf "%s\n" s; flush stdout;;*)
let pi s i = Printf.printf "%s : %i\n" s i; flush stdout;;
(*let ps s = Printf.printf "A - %s\n" s; flush stdout;;*)

let pauto a =
  pi "Init" a.init;
  print_string "Final : [|"; Array.iter (fun b -> print_string (if b then "true " else "false ")) a.final; print_string "|]\n";
  Printf.printf "[|\n[|%s|]\n|]\n"
    (String.concat "|];\n[|"
       (Array.to_list (Array.map
			 (fun t -> String.concat "; " (Array.to_list (Array.map string_of_int t)))
			 a.delta)));;
(* ******************************** *)


let reachable_states a =
  let res = Array.make (size a) false in
  let rec explore s =
    if not res.(s)
    then begin
      res.(s) <- true;
      Array.iter explore a.delta.(s)
    end in
  explore (initState a);
  res;;

let size_boolarray t =
  let i = ref 0 in
  for j = 0 to (Array.length t) - 1 do
    if t.(j) then incr i
  done;
  !i;;

(* supprime les états inateignables *)
let remove_unreachable auto =
  let n = (size auto) in
  let reach = reachable_states auto in
  let i = ref (-1) in
  let inv_table = Array.make (size_boolarray reach) (-1) in
  let table = Array.init n
    (fun s -> if reach.(s)
      then begin
	incr i;
	inv_table.(!i) <- s;
	!i
      end
      else -1) in
  let new_n = !i + 1 in
  let new_init = table.(initState auto) in
  let new_final = Array.init new_n (fun j -> (isFinal auto) (inv_table.(j))) in
  let new_delta = Array.init new_n
    (fun s -> Array.init (sigma auto)
      (fun alpha -> table.(delta auto inv_table.(s) alpha))) in
  { init = new_init; final = new_final; delta = new_delta };;

(* un map qui modifie en place le vecteur en entrée *)
let array_savemap f t =
  for i = 0 to (Array.length t) - 1 do
    t.(i) <- f t.(i)
  done;
  t;;



let invDeltaArray auto =
  let res = Array.make_matrix (size auto) (sigma auto) [] in
  let store s1 a s2 = res.(s2).(a) <-  s1::res.(s2).(a) in
  Array.iteri (fun s v -> Array.iteri (store s) v) (deltaArray auto);
  array_savemap (array_savemap Common.remove_duplicate) res;;


let minimize_partition (auto : t) =
  let invDelta = invDeltaArray auto in
  let n = size auto in
  let k = sigma auto in
  let p = Refine.make n in 
  (* création de la partition F *)
  Array.iteri (fun i final -> if final then Refine.mark p i) auto.final;
  let s0 = Refine.set p 0 in
  let s1 = Refine.split p s0 in

  if s1 <> -1 then
    begin
      let w = Stack.create () in
      Stack.push s1 w;
      while not (Stack.is_empty w) do
	let a = Stack.pop w in
	for c = 0 to k - 1 do
	  let x = ref [] in
	  let touched = ref [] in
	  Refine.iter p (fun i -> x := (invDelta.(i).(c)) :: !x) a;
	  let traite e =
	    touched := (Refine.set p e) :: !touched;
	    Refine.mark p e in
	  List.iter (List.iter traite) !x;
	  List.iter
	    (fun i ->
	      let j = Refine.split p i in
	      if j <> -1 then Stack.push j w
	    )
	    !touched
	done
      done
    end;
  Refine.sets p, Refine.set p, Refine.first p;;

let minimize (auto0 : t) : t =
  let auto = remove_unreachable auto0 in
  let new_n, new_of_old, old_of_new =  minimize_partition auto in
  let old_delta = deltaArray auto in
  let k = sigma auto in
  let new_delta = Array.make_matrix new_n k (-1) in
  for s = 0 to new_n - 1 do
    for alpha = 0 to k - 1 do
      new_delta.(s).(alpha) <- new_of_old (old_delta.(old_of_new s).(alpha))
    done
  done;
  let new_final = Array.init new_n (fun i -> isFinal auto (old_of_new i)) in
  let new_init = new_of_old (initState auto) in
  { init = new_init; final = new_final; delta = new_delta};;

(* return the list of states with no path to a final state*)
(* if the automaton is minimal, this list has length 0 or 1 *)
let trash auto =
  let invdelta = invDeltaArray auto in
  let seen = Array.make (size auto) false in
  let rec explore s =
    if not seen.(s)
    then begin
      seen.(s) <- true;
      Array.iter (List.iter explore) invdelta.(s)
    end in
  Array.iteri (fun i b -> if b then explore i) auto.final;
  let res = ref [] in
  Array.iteri (fun i b -> if not b then res := i :: !res) seen;
  !res;;  
  
(* construction d'automates *)
module Int =
struct
  type t = int
  let compare = compare
end;;
module IntSet = Set.Make (Int);;
module IntSetMap = Map.Make (IntSet);;

let intset_of_intlist l =
  List.fold_right IntSet.add l IntSet.empty;;

(* init, final, delta*)
type nondeterministic = { nd_init : state list;
			  nd_final : bool array;
			  nd_delta : state list array array }

let nd_size a = Array.length a.nd_delta;;
let nd_sigma a = Array.length a.nd_delta.(0);;

let nondeterministic_of_t a =
  let singleton i = [i] in
  { nd_init = singleton a.init;
    nd_final = a.final;
    nd_delta = Array.map (Array.map singleton) a.delta }

let determinize a =
  let deltaset = Array.map (Array.map intset_of_intlist) a.nd_delta in
  let k = nd_sigma a in
  let w = ref IntSetMap.empty in
  let predelta = ref []
  and prefinal = ref [] in
  let nodecount = ref (-1) in
  let new_id () = incr nodecount; !nodecount in

  (* explore s renvoie l'identifiant de s *)
  let rec explore s =
    if (IntSetMap.mem s !w)
    then IntSetMap.find s !w
    else
      begin
	let i = new_id () in
	w := IntSetMap.add s i !w;
	let sucs = Array.make k (-1) in
	for c = 0 to k-1 do
	  let suc = IntSet.fold (fun e acc -> IntSet.union deltaset.(e).(c) acc) s IntSet.empty in
	  sucs.(c) <- explore suc
	done;
	predelta := (i, sucs) :: !predelta;
	prefinal := (i, IntSet.fold (fun i acc -> (a.nd_final.(i)) || acc) s false)  :: !prefinal;
	i
      end in
  let new_init = explore (intset_of_intlist a.nd_init) in
  let new_n = !nodecount + 1 in
  let new_delta = Array.make new_n [||] in
  List.iter (fun (i, t) -> new_delta.(i) <- t) !predelta;
  let new_final = Array.make new_n false in
  List.iter (fun (i, b) -> new_final.(i) <- b) !prefinal;
  { init = new_init; final = new_final; delta = new_delta };;

let make init final delta =
  { init = init; final = final; delta = delta }

let make_nondeterministic init finals_list delta =
  let finals_array = Array.make (Array.length delta) false in
  List.iter (fun i -> finals_array.(i) <- true) finals_list;
  { nd_init = init; nd_final = finals_array; nd_delta = delta };;

let nd_empty sigma =
  { nd_init = [0];
    nd_final = [|false|];
    nd_delta = Array.make_matrix 1 sigma [] }

let nd_epsilon sigma =
  { nd_init = [0];
    nd_final = [|true|];
    nd_delta = Array.make_matrix 1 sigma [] }

let make_setstar_naive ?k l =
  let new_k = match k with
    |Some i -> i
    |None -> (List.fold_left (List.fold_left max) (-1) l) + 1 in
  let n = List.fold_left (fun acc w -> acc + (List.length w) - 1) 1 l in
  let count = ref (-1) in
  let new_id () = incr count; !count in
  let init_state = new_id () in
  let final = Array.make n false in
  final.(init_state) <- true;
  let init = [init_state] in
  let delta = Array.make_matrix n new_k [] in
  let rec aux state = function
    |[]   -> failwith "toto"
    |[c]  -> delta.(state).(c) <- init_state::delta.(state).(c)
    |c::q -> let new_state = new_id () in
	     delta.(state).(c) <- new_state::delta.(state).(c);
	     aux new_state q in
  List.iter (aux init_state) l;
  { nd_init = init; nd_final = final; nd_delta = delta };;


let confuse_letters a letters =
  let new_delta = Array.map Array.copy a.nd_delta in
  let new_final = Array.copy a.nd_final in
  let n = nd_size a in
  for i = 0 to n-1 do
    let new_suc = Common.remove_duplicate (List.concat (List.map (fun c -> new_delta.(i).(c)) letters)) in
    List.iter (fun c -> new_delta.(i).(c) <- new_suc) letters
  done;
  { nd_init = a.nd_init; nd_final = new_final; nd_delta = new_delta };;


let is_safe a =
  let fail s = failwith ("Is safe : "^s) in
  if a.init < 0 then fail "init<0";
  let n = Array.length a.delta in
  if a.init >= n then fail "init>maxi";
  if Array.length a.delta <> Array.length a.final then fail "final";
  let k = Array.length a.delta.(0) in
  for i = 0 to n - 1 do
    if Array.length a.delta.(i) <>k then fail "trans";
    for j = 0 to k - 1 do
      let s = a.delta.(i).(j) in
      if s < 0 || s >= n then fail ("trans state "^(string_of_int s))
    done
  done;;

(* opérations rationnelles sur les automates *)
let complement a =
  { init = a.init;
    final = Array.map not a.final;
    delta = a.delta }

let intersection a b =
  assert (sigma a = sigma b);
  let n, m = size a, size b in
  let encode i j = i + n*j
  and decode k = k mod n, k/n in
  let delta k alpha =
    let i, j = decode k in
    encode a.delta.(i).(alpha) b.delta.(j).(alpha) in
  let final k =
    let i, j = decode k in
    a.final.(i) && b.final.(j) in
  {
    init = encode a.init b.init;
    final = Array.init (n*m) final;
    delta = Array.init (n*m) (fun k -> Array.init (sigma a) (delta k))
  }

let union a b =
  complement (intersection (complement a) (complement b))

(* fermeture transitive d'une relation représentée comme un tableau t *)
(* où t.(i) est la liste des successeurs de i *)
let fermeture_transitive r0 =
  let r = Array.copy r0 in
  let n = Array.length r in
  let fini = ref true in
  while not !fini do
    fini := true;
    for i = 0 to n-1 do
      let l = Common.remove_duplicate
	(List.concat (List.map (fun j -> r.(j)) r.(i))) in
      if l <> r.(i) then
	begin 
	  fini := false;
	  r.(i) <- l
	end
    done
  done;
  r;;

let epsilon_transition a r0 =
  let n = nd_size a in
  let r = fermeture_transitive r0 in
  let delta i alpha =
    let l = List.map (fun j ->
      a.nd_delta.(j).(alpha)) r.(i) in
    Common.remove_duplicate (List.concat l) in
  let final i = List.exists (fun j -> a.nd_final.(j)) r.(i) in
  {
    nd_init = a.nd_init;
    nd_final = Array.init n final;
    nd_delta = Array.init n
      (fun i -> Array.init (nd_sigma a) (delta i))
  }

(* dessin à coté deux automates, l'état initial est celui *)
(* du premier automate *)
let merge a b =
  assert (nd_sigma a = nd_sigma b);
  let n, m = nd_size a, nd_size b in
  let new_a i = i
  and new_b i = n + i in
  let f fa fb i = if i < n then fa i else fb (i - n) in
  let c =
    {
      nd_init = a.nd_init;
      nd_final = Array.init (n + m)
	(f (Array.get a.nd_final) (Array.get b.nd_final));
      nd_delta =
	Array.init (n + m)
	(fun i -> Array.init (nd_sigma a)
	  (fun alpha -> 
	    f (fun ia -> a.nd_delta.(ia).(alpha))
	      (fun ib -> List.map new_b b.nd_delta.(ib).(alpha))
	    i))
    } in
  (c, new_a, new_b);;

let list_of_boolarray t =
  let res = ref [] in
  Array.iteri (fun i b -> if b then res := i :: !res) t;
  !res

let nd_star a =
  let eps = Array.init (nd_size a) 
    (fun i -> i :: if a.nd_final.(i) then a.nd_init else []) in
  epsilon_transition a eps

let nd_concat a b =
  let c, _, new_b = merge a b in
  let init_b = List.map new_b b.nd_init in
  let eps = Array.init (nd_size c) (fun i -> [i]) in
  for i = 0 to (nd_size a)-1 do
    if a.nd_final.(i) then eps.(i) <- i::init_b;
    c.nd_final.(i) <- false
  done;
  epsilon_transition c eps;;

let nd_union a b =
  let c, _, new_b = merge a b in
  {
    nd_init = a.nd_init @ (List.map new_b b.nd_init);
    nd_final = c.nd_final;
    nd_delta = c.nd_delta
  }

let nd_letterSet sigma isInSet =
  let delta = Array.make_matrix 2 sigma [] in
  for a = 0 to sigma - 1 do
    if isInSet a then delta.(0).(a) <- [1]
  done;
  {
    nd_init = [0];
    nd_final = [|false; true|];
    nd_delta = delta
  }


let nd_letter sigma a =
  nd_letterSet sigma ((=) a)
  
let nd_rev a =
  let n = nd_size a
  and sigma = nd_sigma a in
  let init = list_of_boolarray a.nd_final in
  if init = [] then nd_empty sigma
  else
    begin
      let final = Array.make n false in
      List.iter (fun i -> final.(i) <- true) a.nd_init;
      let delta = Array.make_matrix n sigma [] in
      for i = 0 to n-1 do
	for alpha = 0 to sigma-1 do
	  List.iter
	    (fun j -> delta.(j).(alpha) <- i::delta.(j).(alpha))
	    a.nd_delta.(i).(alpha)
	done
      done;
      { nd_init = init; nd_final = final; nd_delta = delta }
    end

(* ************ automates non complets ************* *)
type 'a noncomplete = { nc_init : state; nc_final : bool array; nc_delta : ('a * state) list array }

let nc_size a = Array.length a.nc_final

let make_noncomplete init final delta = { nc_init = init; nc_final = final; nc_delta = delta }

let pnc p a =
  pi "Init" a.nc_init;
  print_string "Final :";
  Array.iteri (fun i b -> if b then Printf.printf " %d" i) a.nc_final;
  print_newline ();
  Array.iteri
    (fun i s -> Printf.printf "\nstate %d :" i;
      List.iter (fun (a, j) -> Printf.printf "\n  -%s-> %d" (p a) j) s)
    a.nc_delta;
  print_string "\n\n";;

let drop_noncomplete a = a.nc_init, a.nc_final, a.nc_delta

let remove_states toBeRemoved auto =
  (* attribution des nouveaux numéros d'état *)
  let n = Array.length toBeRemoved
  and new_n = ref 0 in
  let dico = Array.make n (-1) in
  for i = 0 to n-1 do
    if not toBeRemoved.(i) then
      begin
	dico.(i) <- !new_n;
	incr new_n
      end      
  done;
  let new_init = dico.(auto.nc_init)
  and new_final = Array.make !new_n false
  and new_delta = Array.make !new_n [] in
  let filter (_, s) = not toBeRemoved.(s)
  and map (alpha, s) = (alpha, dico.(s)) in
  for i = 0 to n-1 do
    if not toBeRemoved.(i) then
      begin
	new_final.(dico.(i)) <- auto.nc_final.(i);
	new_delta.(dico.(i)) <- List.map map (List.filter filter auto.nc_delta.(i))
      end
  done;
  { nc_init = new_init; nc_final = new_final; nc_delta = new_delta };;

let remove_unreachable_nc auto =
  let toBeRemoved = Array.make (Array.length auto.nc_delta) true in
  let rec explore s =
    if toBeRemoved.(s) then 
      begin
	toBeRemoved.(s) <- false;
	List.iter (fun (_, t) -> explore t) auto.nc_delta.(s)
      end
  in
  explore auto.nc_init;
  remove_states toBeRemoved auto;;



(* *)
let makeAssoc l =
  let rec aux res acc_el = function
    | (k, a)::((l, _)::_ as q) ->
      if k = l then aux res (a::acc_el) q  
      else aux ((k, a::acc_el)::res) [] q
    | [k, a] -> (k, a::acc_el)::res
    | [] -> [] in
  let sorted_l = List.sort (fun (a, _) (b, _) -> compare a b) l in
  aux [] [] sorted_l;;

let mergeAssocs l =
  List.map (fun (k, m) -> (k, List.concat m))
    (makeAssoc (List.concat l));;


(* *)
type ('a, 'b) tree =
| N of 'a * ('b * ('a, 'b) tree) list

(* supprime les listes vides présentes au début de la liste d'entrée *)
(* le booléen reenvoyé indique si de telles listes ont été trouvées ou non *)
let remove_emptylists l =
  let rec aux changed = function
    | [] :: q -> aux true q
    | l -> l, changed in
  aux false (List.sort compare l);;

let sort_by_first l =
  let assoc = List.map (function t::q -> (t,q) | [] -> failwith "sort_by_first") l in
  makeAssoc assoc;;

let tree_of_list list =
  let sortedList = List.sort compare list in
  let rec aux l0 =
    let l, isFinal = remove_emptylists l0 in
    let assoc = sort_by_first l in
    let sons = List.rev_map (fun (t, q) -> (t, aux q)) assoc in
    N (isFinal, sons) in
  aux sortedList;;

let nc_of_tree tree =
  let new_id =
    let i = ref (-1) in
    fun () -> incr i; !i in
  let final = ref []
  and trans = ref [] in
  let addFinal i = final := i :: !final
  and addTrans i alpha j = trans := (i, (alpha, j)) :: !trans in
  let init = new_id () in
  let rec explore i = function
    | N (isFinal, l) ->
      if isFinal then addFinal i;
      List.iter
	(fun (alpha, t) ->
	  let j = new_id () in
	  addTrans i alpha j;
	  explore j t) l in
  explore init tree;
  (* construction of the imperative structure *)
  let n = new_id () in
  let nc_final = Array.make n false
  and nc_delta = Array.make n [] in
  List.iter (fun i -> nc_final.(i) <- true) !final;
  List.iter (fun (i,t) -> nc_delta.(i) <- t :: nc_delta.(i)) !trans;
  { nc_init = init; nc_final = nc_final; nc_delta = nc_delta };;

(* nc_finite_set l make an automaton that recognizes the words in l *)
let nc_finite_set l =
  nc_of_tree (tree_of_list l);;

let nc_used_letters a =
  let res = ref [] in
  let explore (alpha, _) = res := alpha :: !res in
  Array.iter (List.iter explore) a.nc_delta;
  Common.remove_duplicate !res;;

let t_of_nc ?sigma:sigma0 a =
  let sigma = match sigma0 with
    | None -> nc_used_letters a
    | Some s -> s in
  let dico = Minidata.makeToInt sigma in
  let n = nc_size a in
  let new_n = n + 1 in
  let final = Array.make new_n false in
  let delta = Array.make_matrix new_n (Minidata.size dico) n in
  for i = 0 to n - 1 do
    final.(i) <- a.nc_final.(i)
  done;
  for i = 0 to n - 1 do
    List.iter
      (fun (alpha, j) -> delta.(i).(Minidata.to_int dico alpha) <- j)
      a.nc_delta.(i)
  done;
  ({ init = a.nc_init ; final = final ; delta = delta }, dico);;


(* minimisation d'automate non-complet (mais déterministe) *)
let invDeltaList delta =
  let n = Array.length delta in
  let res = Array.make n [] in
  for i = 0 to n-1 do
    List.iter (fun (alpha, s) -> res.(s) <- (alpha, i)::res.(s)) delta.(i)
  done;
  Array.map makeAssoc res;;

let minimize_partition_noncomplete auto =
  let invDelta = invDeltaList auto.nc_delta in
  let n = Array.length auto.nc_delta in
  let p = Refine.make n in 
  (* création de la partition F *)
  Array.iteri (fun i final -> if final then Refine.mark p i) auto.nc_final;
  let s0 = Refine.set p 0 in
  let s1 = Refine.split p s0 in
  
  let s2 = if s1 <> -1 then s1 else s0 in
  begin
    let w = Stack.create () in
    Stack.push s2 w;
    while not (Stack.is_empty w) do
      let a = Stack.pop w in
      let x = ref [] in
      Refine.iter p (fun i -> x := invDelta.(i) :: !x) a;
      let new_x = mergeAssocs !x in
      
      let compute_pred (_, predlist) =
	(* marquage *)
	let touched = ref [] in
	let traite e =
	  touched := (Refine.set p e) :: !touched;
	  Refine.mark p e in
	List.iter traite predlist;
	(* sissions *)
	List.iter
	  (fun i ->
	    let j = Refine.split p i in
	    if j <> -1 then Stack.push j w
	  )
	  !touched
      in
      List.iter compute_pred new_x
    done
  end;
  Refine.sets p, Refine.set p, Refine.first p;;

let nc_minimize a0 =
  let a = remove_unreachable_nc a0 in
  let new_n, new_of_old, old_of_new = minimize_partition_noncomplete a in
  let new_delta = Array.make new_n []
  and new_final = Array.make new_n false
  and new_init = new_of_old a.nc_init in
  for i = 0 to new_n-1 do
    new_final.(i) <- a.nc_final.(old_of_new i);
    new_delta.(i) <- List.map (fun (a, s) -> (a, new_of_old s)) a.nc_delta.(old_of_new i)
  done;
  { nc_init = new_init; nc_final = new_final; nc_delta = new_delta };;  


(* ************* *)
let reconnait auto l =
  isFinal auto (List.fold_left (delta auto) (initState auto) l);;

let generate_p out pstate auto min max =
  let k = sigma auto in
  let rec aux i acc state =
    if isFinal auto state && i <= max - min then
      begin
	List.iter (fun i -> Printf.fprintf out "%s " (pstate i)) (List.rev acc);
	Printf.fprintf out "\n";
	flush out;
      end;
    if i > 0 then
      for j = 0 to k - 1 do
	aux (i-1) (j::acc) ((delta auto) state j)
      done in
  aux max [] (initState auto);;

let generate_f name pstate auto a b =
  let out = open_out name in
  generate_p out pstate auto a b;
  close_out out;;

let random_automaton n k : t =
  let init = Random.int n
  and final = Array.init n (fun _ -> Random.bool ())
  and delta = Array.init n (fun _ -> Array.init k (fun _ -> Random.int n)) in
  { init = init; final = final; delta = delta };;

let random_nondeterministic n k =
  let init = [Random.int n; Random.int n; Random.int n; Random.int n; Random.int n; Random.int n; Random.int n; Random.int n]
  and final = Array.init n (fun _ -> Random.int 20 = 0)
  and delta = Array.init n (fun _ -> Array.init k (fun _ -> [Random.int n; Random.int n; Random.int n; Random.int n; Random.int n])) in
  { nd_init = init; nd_final = final; nd_delta = delta };;

