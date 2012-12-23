(* un module moins exact que "pieds" pour faire des vers libres d'une longueur approximative donnée *)

type rules = int

module C =
struct
  type metadata = int
  type state = int

  let precompute _ _ _ s = (String.length s) + 1

  let filter _ _ = true

  let step i j = i - j

  let init_state () = 0

  (*pretty-printing*)
  let name = "Libre"
  let print_state = string_of_int
  let print_metadata = string_of_int
end

type order = rules

let use_order _ o = o

let finished i = i <= 0;;
