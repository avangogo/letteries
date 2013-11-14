open Common

(* le type source *)
type source =
  {
    text : string option;
    book : string option;
    author : string option
  }

let make_source ?author ?text ?book () =
  {
    author = author;
    text = text;
    book = book
  }  

(* fonctions d'écritures/lecture des type source dans un fichier *)
(* ces fonctions doivent être inverses l'une de l'autre *)
let sprint_opt = function
  | None -> "\"\""
  | Some s -> Printf.sprintf "\"%s\"" s

let scan_opt = function
  | "" -> None
  | s -> Some s

let sprint_source s =
  Printf.sprintf "< source author=%s text=%s book=%s \\>\n"
    (sprint_opt s.author) (sprint_opt s.text) (sprint_opt s.book)

let read_source s =
  let input = Scanf.Scanning.from_string s in
  let make a t b =
    { author = scan_opt a; text = scan_opt t; book = scan_opt b } in
  Scanf.bscanf input "< source author = %S text = %S book = %S \\>" make


let test = { author = Some "aG\nrzac eeff"; text = Some "'"; book = None };;
assert ((read_source (sprint_source test)) = test);;

(* le type word *)
type word =
  {
    word : string;
    tag : Tag.tag;
    lemma : string;
    file : string;
    source : source;
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
    source = make_source ();
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
    source = w.source;
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
