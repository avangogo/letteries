exception UnKnownTag of string

type tag

val string_of_tag : tag -> string
val tag_of_string : string -> tag

val all : tag array
val nbre : int

val tag_of_int : int -> tag
val int_of_tag : tag -> int

val default : tag
val sent : tag

val isRelevant : tag -> bool
