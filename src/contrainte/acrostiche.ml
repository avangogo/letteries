open Word

module C =
struct
  type metadata = char

  type state = char

  let precompute w = w.word.[0]

  let final _ = true

  let step _ c = c
  
  let init_state () = '0'

  let name = "Acrostiche"

  let print_metadata c = String.make 1 c

  let print_state c = String.make 1 c
end

type order = char

let use_order c o =
  if c <> o then raise Contrainte.ContrainteNonRespectee
  else o
