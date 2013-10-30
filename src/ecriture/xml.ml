open Word

let field sep balise text =
  Printf.sprintf "<%s>%s%s%s</%s>" balise sep text sep balise;;

let xml_of_word w =
  (field "\n" "word"
    (String.concat "\n"
       [ 
	 field "" "token" w.word;
	 field "" "tag" (Tag.string_of_tag w.tag);
	 field "" "lemma" w.lemma;
	 field "" "file" w.file;
	 field "" "phonetic" (String.concat ""
				(List.map
				   Phonetique.string_of_phoneme
				   w.phonetic));
	 field "" "relevant" (string_of_bool w.relevant)
       ]))^"\n"
  
