open Common

(* le type word *)
type word =
  {
    word : string;
    tag : Tag.tag;
    lemma : string;
    file : string;
    phonetic : Phonetique.phoneme list;
    relevant : bool
  }

let state_of_word w =
  State.make (w.word, w.tag)

let make_word s =
  {
    word = s;
    tag = Tag.SYM;
    lemma = s;
    file = "";
    phonetic = [];
    relevant = false
  }

let set_word w s =
  if s = w.word then w
  else
    {
    word = s;
    tag = w.tag;
    lemma = w.lemma;
    file = w.file;
    phonetic = w.phonetic;
    relevant = w.relevant
    }

(* le type sep *)
type sep =
|Space
|Newline

let string_of_sep = function
  |Space -> " "
  |Newline -> "\n"

let sep_assoc =
  [ ' ', Space;
    '\n', Newline ];;

(* le type token *)
type token = (sep, word) sum

let token_of_word w = R (w:word)

let token_of_sep s = L (s:sep)

let string_of_token = function
  | R w -> w.word
  | L sep -> string_of_sep sep;;
