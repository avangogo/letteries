let automaton = ref (fun (_: string) -> failwith "unassigned automaton")

let emptystate = Phonetique.phoneme_vide

module C =
struct
  type metadata = Phonetique.phoneme * int * Phonetique.muet
  type state = Phonetique.phoneme * int

  let precompute _ _ _ s =
    let phonetique = (!automaton s) in
    let debut = match phonetique with
      |t::_ -> t
      |[]   -> Phonetique.phoneme_vide in
    debut, Phonetique.nbre_voyelles phonetique, Phonetique.fin_muette phonetique 
  let int_of_liaison muet debut =
    Phonetique.nbre_voyelles (Phonetique.liaison muet debut)

  let filter (debut, i) (_, j, muet) = i >= j + (int_of_liaison muet debut)(*FIXME: optimiser?*)

  let step (debut, i) (debut2, j, muet) =
    let new_i = i - (j + (int_of_liaison muet debut)) in
    if i < 0 then raise Contrainte.ContrainteNonRespectee else
      (debut2, new_i)

  let init_state () = (Phonetique.phoneme_vide, 0)

  (*pretty-printing*)
  let name = "Pieds";;
  let print_state (phoneme, i) = Printf.sprintf "(%s, %d)" (Phonetique.string_of_phoneme phoneme) i;;
  let print_metadata (phoneme, i, muet) = Printf.sprintf "(%s, %d%, %s)" (Phonetique.string_of_phoneme phoneme) i (Phonetique.string_of_muet muet);;
end;;

type order =
  |Newline of int
  |Cesure of int;;
let use_order (p,_) = function
  |Newline i -> Phonetique.phoneme_vide, i
  |Cesure i -> p, i;;
let finished (_, i) = i = 0;;
