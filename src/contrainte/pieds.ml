open Word

type rules =
  |Newline of int
  |Cesure of int

module C =
struct
  type metadata = Phonetique.phoneme * int * Phonetique.muet
  type state = Phonetique.phoneme * int

  let precompute w =
    let debut = match w.phonetic with
      |t::_ -> t
      |[]   -> Phonetique.phoneme_vide in
    debut, Phonetique.nbre_voyelles w.phonetic, Phonetique.fin_muette w.phonetic 
  let int_of_liaison muet debut =
    Phonetique.nbre_voyelles (Phonetique.liaison muet debut)

  let final _ = true

  let step (debut, i) (debut2, j, muet) =
    let new_debut = if debut2 <> Phonetique.phoneme_vide then debut2 else debut in
    let new_i = i - (j + (int_of_liaison muet debut)) in
    if new_i < 0 then raise Contrainte.ContrainteNonRespectee else
      (new_debut, new_i)

  let init_state () = (Phonetique.phoneme_vide, 0)

  (*pretty-printing*)
  let name = "Pieds";;
  let print_state (phoneme, i) = Printf.sprintf "(%s, %d)" (Phonetique.string_of_phoneme phoneme) i;;
  let print_metadata (phoneme, i, muet) = Printf.sprintf "(%s, %d%, %s)" (Phonetique.string_of_phoneme phoneme) i (Phonetique.string_of_muet muet);;
end;;

type order = rules

let use_order (p,_) = function
  |Newline i -> Phonetique.phoneme_vide, i
  |Cesure i -> p, i;;
let finished (_, i) = i = 0;;
