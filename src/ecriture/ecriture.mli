val ponctuation_liante : char list
val ponctuation_finale : char list
val ponctuation_espace : char list
val ponctuation_capitalisante : char list

type type_carac = PonctuationLiante | PonctuationFinale | Espace | Lettre
val type_carac_of_char : char -> type_carac
val list_iteri : (int -> 'a -> 'b) -> 'a list -> unit
val string_of_char_list : char list -> string
val reparse : string list -> string list
val gere_sauts : string -> string
val met_en_page : int -> string list -> int
val string_end : string -> char
val ajoute_majuscules : string list -> string list
val affiche : out_channel -> string list -> unit
val affiche_poeme : ?out:out_channel -> string list -> unit
