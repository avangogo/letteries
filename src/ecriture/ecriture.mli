open Word

val ponctuation_liante : char list
val ponctuation_finale : char list
val ponctuation_espace : char list
val ponctuation_capitalisante : char list

type type_carac = PonctuationLiante | PonctuationFinale | Espace | Lettre
val type_carac_of_char : char -> type_carac
(* val reparse : string list -> string list *)
(* val gere_sauts : string -> string *)
(* val met_en_page : int -> string list -> int*)
(*val ajoute_majuscules : string list -> string list*)

val reparse_words : word list -> token list
val capitalize : token list -> token list
val reparse : word list -> token list
