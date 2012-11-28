exception UnKnownTag of string
type detTag = ART | Pos
type proTag = DEM | IND | PER | POS | REL
type prpTag = Det
type punTag = Cit
type verTag =
    Cond
  | Futu
  | Impe
  | Impf
  | Infi
  | Pper
  | Ppre
  | Pres
  | Simp
  | Subi
  | Subp
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

val string_of_tag : tag -> string
val tag_of_string : string -> tag

val all : tag array
val nbre : int

val tag_of_int : int -> tag
val int_of_tag : tag -> int
