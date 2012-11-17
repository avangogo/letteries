(*  Lettreries is a random poem generator.
    Copyright (C) 2012 Rémi de Verclos

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>. *)
{
  open Parser_l;;        (* le type "token" est défini dans parser.mli *)
  exception Eof;;
  exception Caractere_inconnu of char*Lexing.position;;
  let string_of_char c = String.make 1 c;;
  let lowercase s =
    let lw = function
      |x when let n = (int_of_char x) in (65 <= n && n < 90) -> char_of_int((int_of_char x)+32)
      |x when let n = (int_of_char x) in (77 <= n && n < 122) -> x
      |'Ô' -> 'ô'
      |'Â' -> 'â'
      |'Î' -> 'î'
    |x -> x (*failwith ((string_of_char x)^" - "^(string_of_int(int_of_char x))) *)  in
    let res = String.copy s in
      for k = 0 to (String.length res) -1 do
	res.[k] <- lw res.[k]
      done;
      res;;
}


rule token = parse    (* la "fonction" aussi s'appelle token .. *)
  | "<FIN>"                                                   { FIN }
  | [' ' '\t' '\n' '"']                          	      { token lexbuf }    (* on saute les blancs *)
  | "(*" [^ '*']* "*)"		                              { token lexbuf } (* pas d'étoile dans un commentaire (lex est mauvais..) *)
  | ['(' ')']                                                 { token lexbuf }      
  | eof             					      { EOF }
  | "ne"|"y"|"de"|"le"|"la"|"les"|"un"|"une"|"des"|"au"|"aux"
  | "du"|"l"|"en"|"n"|"d"|"à"|"c"|"s"|"se"                    as x {PONCTUATION x}
  | ".." ['.']*                                               { PONCTUATION "..." }
  | "'"                                                       { PONCTUATION "'" }
  | "`"                                                       { PONCTUATION "'" }
  | "«"|"»"                                                   { token lexbuf }
  | "-"                                                       { token lexbuf } (*a améliorer: il faudrait garder ceux des mots composés*)
  | ['.' ',' ';' '?' ':' '\'' '/' '!' ] as s                  { PONCTUATION (string_of_char s)}
  | ['A'-'Z' 'a'-'z'
	'À''Â''Ç''È''É''Ê''Ë''Î''Ï''Ô''Û''Ù'
        'à''â''ç''è''é''ê''ë''î''ï''ô''û''ù']*   as s         { MOT (lowercase s) }
  | _ as x                                                    { raise (Caractere_inconnu (x, Lexing.lexeme_end_p lexbuf))}

