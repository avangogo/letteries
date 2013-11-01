val toBeUpdate : string list -> string -> bool

val make : string option -> string list -> string -> (unit -> 'a) -> unit
      
val load :
  ?makeMessage:string ->
  ?loadMessage:string ->
  string list -> string -> (unit -> 'a) -> 'a
