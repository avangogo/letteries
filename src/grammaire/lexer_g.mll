{
  open Parser_g
  open Lexing
}

let id = ['a'-'z']['a'-'z' 'A'-'Z' '_' '0'-'9']*
 
rule token = parse
  | '\n' 							{ new_line lexbuf; token lexbuf }
  | [' ' '\t']	                         	                { token lexbuf }
  | '|'  							{ PIPE }
  | '*'								{ STAR }
  | (id as s)[' ' '\t' '\n']*':'     				{ IDPOINTS s }
  | ['A'-'Z']['a'-'z' 'A'-'Z' ':']* as s                        { TAG (Tag.tag_of_string s) }
  | id as s		        				{ ID s }
  | eof             					        { EOF }
  | _                                                      {
	let p	 = lexeme_start_p lexbuf in
	failwith (Printf.sprintf "Ligne %d, caractère %d : caractère inconnu : %s" p.pos_lnum p.pos_cnum (lexeme lexbuf))}

