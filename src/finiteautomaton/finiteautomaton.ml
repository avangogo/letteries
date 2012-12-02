(* Affichage, debuggage *)
let p s = Printf.printf "%s\n" s; flush stdout;;
let pi s i = Printf.printf "%s : %i\n" s i; flush stdout;;
let ps s = Printf.printf "A - %s\n" s; flush stdout;;

let pauto (init, final, delta)=
  pi "Init" init;
  print_string "Final : [|"; Array.iter (fun b -> print_string (if b then "true " else "false ")) final; print_string "|]\n";
  Printf.printf "[|\n[|%s|]\n|]\n"
    (String.concat "|];\n[|"
       (Array.to_list (Array.map
			 (fun t -> String.concat "; " (Array.to_list (Array.map string_of_int t)))
			 delta)));;


(* *)
type state = int
type sigma = int

(* etat init, etats finaux, transitions*)
type t = state * (bool array) * (state array array)

(* fonctions générales *)
let sigma        ((_ , _, d) : t) = Array.length d.(0)
let size         ((_ , _, d) : t) = Array.length d
let initState    ((s , _, _) : t) = s
let isFinalArray ((_ , f, _) : t) = f
let deltaArray   ((_ , _, d) : t) = d

let isFinal a i = (isFinalArray a).(i)
let delta a (state : state) (letter : sigma) = (deltaArray a).(state).(letter)

let empty = 0, [|false|], [||]

let reachable_states auto =
  let res = Array.make (size auto) false in
  let delta = deltaArray auto in
  let rec explore s =
    if not res.(s)
    then begin
      res.(s) <- true;
      Array.iter explore delta.(s)
    end in
  explore (initState auto);
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
  new_init, new_final, new_delta;;

(* un map qui modifie en place le vecteur en entrée *)
let array_savemap f t =
  for i = 0 to (Array.length t) - 1 do
    t.(i) <- f t.(i)
  done;
  t;;

let supprime_doubles l =
  let rec aux = function
    |a::b::q -> if a = b
      then aux (a::q)
      else a:: aux (b::q)
    |[a] -> [a]
    |[] -> [] in
  aux (List.sort compare l);;

let invDeltaArray auto =
  let res = Array.make_matrix (size auto) (sigma auto) [] in
  let store s1 a s2 = res.(s2).(a) <-  s1::res.(s2).(a) in
  Array.iteri (fun s v -> Array.iteri (store s) v) (deltaArray auto);
  array_savemap (array_savemap supprime_doubles) res;;


let minimize_partition (auto : t) =
  let invDelta = invDeltaArray auto in
  let n = size auto in
  let k = sigma auto in
  let p = Refine.make n in 
  (* création de la partition F *)
  Array.iteri (fun i final -> if final then Refine.mark p i) (isFinalArray auto);
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
  new_init, new_final, new_delta;;

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
  Array.iteri (fun i b -> if b then explore i) (isFinalArray auto);
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
type nondeterministic = (state list) * (bool array) * (state list array array)

let nd_size (_, _, delta) = Array.length delta;;
let nd_sigma (_, _, delta) = Array.length delta.(0);;

let determinize ((init, final, delta) as auto : nondeterministic) =
  let deltaset = Array.map (Array.map intset_of_intlist) delta in
  let k = nd_sigma auto in
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
	prefinal := (i, IntSet.fold (fun i acc -> (final.(i)) || acc) s false)  :: !prefinal;
	i
      end in
  let new_init = explore (intset_of_intlist init) in
  let new_n = !nodecount + 1 in
  let new_delta = Array.make new_n [||] in
  List.iter (fun (i, t) -> new_delta.(i) <- t) !predelta;
  let new_final = Array.make new_n false in
  List.iter (fun (i, b) -> new_final.(i) <- b) !prefinal;
  new_init, new_final, new_delta;;

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
  init, final, delta
;;


let confuse_letters ((init, final, delta) as auto) letters =
  let new_delta = Array.map Array.copy delta in
  let new_final = Array.copy final in
  let n = nd_size auto in
  for i = 0 to n-1 do
    let new_suc = supprime_doubles (List.concat (List.map (fun c -> new_delta.(i).(c)) letters)) in
    List.iter (fun c -> new_delta.(i).(c) <- new_suc) letters
  done;
  init, new_final, new_delta;;





let is_safe ((i, fin, delta) : t) =
  let fail s = failwith ("Is safe : "^s) in
  if i < 0 then fail "init<0";
  let n = Array.length delta in
  if i >= n then fail "init>maxi";
  if Array.length delta <> Array.length fin then fail "final";
  let k = Array.length delta.(0) in
  for i = 0 to n - 1 do
    if Array.length delta.(i) <>k then fail "trans";
    for j = 0 to k - 1 do
      let s = delta.(i).(j) in
      if s < 0 || s >= n then fail ("trans state "^(string_of_int s))
    done
  done;;
  



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


(*** tests ***)
(*    0((12)*|2)1    *)
(* init 2 ; 3 : trash *)
let (auto1 : t) =
  2,
  [|false;false;false;true;false;false;true;false|],
  [|
    [|3;4;3|];
    [|3;5;0|];
    [|1;3;3|];
    [|3;3;3|];
    [|3;3;3|];
    [|3;3;6|];
    [|7;5;3|];
    [|3;3;3|]
  |];;

let (auto2:t) =
  0,
  [|false; true; false|],
  [|
    [|2;2;1|];
    [|1;1;2|];
    [|2;2;2|]
  |];;


let random_automaton n k : t =
  let init = Random.int n
  and final = Array.init n (fun _ -> Random.bool ())
  and delta = Array.init n (fun _ -> Array.init k (fun _ -> Random.int n)) in
  init, final, delta;;

let random_nondeterministic n k =
  let init = [Random.int n; Random.int n; Random.int n; Random.int n; Random.int n; Random.int n; Random.int n; Random.int n]
  and final = Array.init n (fun _ -> Random.int 20 = 0)
  and delta = Array.init n (fun _ -> Array.init k (fun _ -> [Random.int n; Random.int n; Random.int n; Random.int n; Random.int n])) in
  init, final, delta;;


let nd1 =
  [1; 3], 
  [4; 0],
  [|
    [|[5;0;1]; []; []|];
    [|[2]; []; []|];
    [|[]; [3]; [0;3]|];
    [|[0;4]; []; [4]|];
    [|[5]; [3]; [4]|];
    [|[5]; [3]; []|]
  |]
;;

Random.self_init ();;
let auto = make_setstar_naive 
  [
    [0;1;2];
    [2;2];
    [3;3;3];
    [4;4;4];
    [5;5;5]
];;
