open Word
open Common

let field sep balise text =
  Printf.sprintf "<%s>%s%s%s</%s>" balise sep text sep balise;;

let field_opt sep balise = function
  | Some s -> field sep balise s
  | None -> ""

let xml_of_word w =
  (field "\n" "word"
    (String.concat "\n"
       (List.filter ((<>) "")
	  [ 
	    field "" "token" w.word;
	    field "" "tag" (Tag.string_of_tag w.tag);
	    field "" "lemma" w.lemma;
	    field "" "file" w.file;
	    field "" "phonetic" (Phonetique.sprettyprint_api_utf8 w.phonetic);
	    field "" "relevant" (string_of_bool w.relevant);
	    field_opt "" "author" w.source.author;
	    field_opt "" "text" w.source.text;
	    field_opt "" "source" w.source.book
	  ])))^"\n"
  
let xml_of_sep = function
  |Space -> field "" "sep" "space"
  |Newline -> field "" "sep" "newline"

let xml_of_token = function
  |R w -> xml_of_word w
  |L s -> xml_of_sep s

let title t = field "" "title" t

let prelude = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<data>"

let epilogue = "</data>"
