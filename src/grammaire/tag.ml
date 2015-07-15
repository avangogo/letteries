(* 
    Lettreries is a random poem generator.
    Copyright (C) 2012 Rémi de Verclos

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

(* formalisation en caml de la sortie du programme TreeTagger *)
(* Ces tags sont les tags français définis ici :
http://www.ims.uni-stuttgart.de/~schmid/french-tagset.html *)

exception UnKnownTag of string

type detTag = ART|Pos
type proTag = DEM|IND|PER|POS|REL
type prpTag = Det
type punTag = Cit
type verTag = Cond|Futu|Impe|Impf|Infi|Pper|Ppre|Pres|Simp|Subi|Subp

type tag =
  |ABR|ADJ|ADV
  |DET of detTag
  |INT|KON|NAM|NOM|NUM
  |PRO of proTag option
  |PRP of prpTag option
  |PUN of punTag option
  |SENT
  |SYM
  |VER of verTag;;

(* ********** *)
let string_of_detTag = function
  |ART -> "ART" |Pos -> "POS"
let string_of_proTag = function
  |DEM -> "DEM" |IND-> "IND" |PER -> "PER"
  |POS -> "POS" |REL -> "REL"
let string_of_prpTag = function
  |Det -> "det"
let string_of_punTag = function
  |Cit -> "cit"
let string_of_verTag = function
  |Cond -> "cond" |Futu -> "futu" |Impe -> "impe" |Impf -> "impf"
  |Infi -> "infi" |Pper -> "pper" |Ppre -> "ppre" |Pres -> "pres"
  |Simp -> "simp" |Subi -> "subi" |Subp -> "subp"
let raw_string_of_tag = function
  |ABR -> "ABR"   |ADJ -> "ADJ"   |ADV -> "ADV"   |DET _ -> "DET"
  |INT -> "INT"   |KON -> "KON"   |NAM -> "NAM"   |NOM -> "NOM"
  |NUM -> "NUM"   |PRO _ -> "PRO" |PRP _ -> "PRP" |PUN _ -> "PUN"
  |SENT -> "SENT" |SYM -> "SYM"   |VER _  -> "VER"

let string_of_tag tag =
  let sep = ":" in
  (^) (raw_string_of_tag tag)
    begin match tag with
      | PRO (Some proTag) -> sep ^ ( string_of_proTag proTag )
      | PRP (Some prpTag) -> sep ^ ( string_of_prpTag prpTag )
      | PUN (Some punTag) -> sep ^ ( string_of_punTag punTag )
      | VER verTag -> sep ^ ( string_of_verTag verTag )
      | DET detTag -> sep ^ ( string_of_detTag detTag )
      | _ -> "" 
    end;;

(* ********** *)
let detTag_of_string = function
  |"ART" -> ART  |"POS" -> Pos |s -> raise (UnKnownTag s)
let proTag_of_string = function
  |"DEM" -> DEM |"IND"-> IND  |"PER" -> PER
  |"POS" -> POS |"REL" -> REL |s -> raise (UnKnownTag s)
let prpTag_of_string = function
  |"det" -> Det |s -> raise (UnKnownTag s)
let punTag_of_string = function
  |"cit" -> Cit |s -> raise (UnKnownTag s)
let verTag_of_string = function
  |"cond" -> Cond |"futu" -> Futu |"impe" -> Impe |"impf" -> Impf
  |"infi" -> Infi |"pper" -> Pper |"ppre" -> Ppre |"pres" -> Pres
  |"simp" -> Simp |"subi" -> Subi |"subp" -> Subp |s -> raise (UnKnownTag s)

let prefix p s =
  let lp = String.length p in
  lp <= String.length s && p = String.sub s 0 lp;;
let f i s = String.sub s i ((String.length s) - i);;

let tag_of_string = function
  |"ABR" -> ABR |"ADJ" ->  ADJ  |"ADV" -> ADV |"INT" -> INT
  |"KON" -> KON |"NAM" ->  NAM  |"NOM" -> NOM |"NUM" -> NUM
  |"SENT" -> SENT |"SYM" -> SYM
  |"PRO" -> PRO None |"PRP" -> PRP None |"PUN" -> PUN None
  |s when prefix "PRO:" s ->
    PRO (Some (proTag_of_string (f 4 s)))
  |s when prefix "PRP:" s ->
    PRP (Some (prpTag_of_string (f 4 s)))
  |s when prefix "PUN:" s ->
    PUN (Some (punTag_of_string (f 4 s)))
  |s when prefix "DET:" s ->
    DET (detTag_of_string (f 4 s))
  |s when prefix "VER:" s ->
    VER (verTag_of_string (f 4 s))
  |s -> raise (UnKnownTag s);;

let all =
  [| ABR; ADJ; ADV; DET ART; DET Pos; INT; KON; NAM; NOM; NUM;
     PRO None; PRO (Some DEM); PRO (Some IND); PRO (Some PER); PRO (Some POS); PRO (Some REL);
     PRP None; PRP (Some Det); PUN None; PUN (Some Cit); SENT; SYM;
     VER Cond; VER Futu; VER Impe; VER Impf; VER Infi; VER Pper; VER Ppre; VER Pres;
     VER Simp; VER Subi; VER Subp |];;

Array.sort compare all;;

let nbre = Array.length all;;

let tag_of_int i =
  try
    all.(i)
  with
    |_ -> failwith (Printf.sprintf "tag_of_int. Invalid argument: %d" i)

let int_of_tag tag =
  let rec aux i j =
    if i+1 = j then
      if all.(i) = tag then i
      else failwith ("int_of_tag : unknown tag : "^(string_of_tag tag))
    else
      let mid = (i+j)/2 in
      if tag < all.(mid)
      then aux i mid
      else aux mid j in
  aux 0 (Array.length all);;

let default = SYM
let sent = SENT

(* say wether the word can be a state of the automaton in the main algorithm *)
let isRelevant = function
  |DET _ -> false
  |PUN _ -> false
  |SENT -> false
  |PRP _ -> false
  |PRO _ -> false
  |ADV -> false
  |INT -> false
  |KON -> false
  |_ -> true;;
