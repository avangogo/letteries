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
  exception Caractere_inconnu of char*Lexing.position;;
}


rule token out = parse    (* la "fonction" aussi s'appelle token .. *)
  | [' ' '\t' '\n'] as c                         	      { output_char out c; token out lexbuf }    (* on saute les blancs *)
  | "(*" [^ '*']* "*)"		                              { token out lexbuf } (* pas d'étoile dans un commentaire (lex est mauvais..) *)
  | ['(' ')']                                                 { token out lexbuf }      
  | ".." ['.']*                                               { output_string out "..." ; token out lexbuf}
  | "'"|"`"|"\'"                                              { output_string out "'" ; token out lexbuf}
  | "«"|"»"|'"'                                               { token out lexbuf }
  | "-"                                                       { token out lexbuf } (*a améliorer: il faudrait garder ceux des mots composés*)
  | ['.' ',' ';' '?' ':' '\'' '/' '!' ] as s                  { output_char out s; token out lexbuf}
  | ['A'-'Z' 'a'-'z'
	'À''Â''Ç''È''É''Ê''Ë''Î''Ï''Ô''Û''Ù'
        'à''â''ç''è''é''ê''ë''î''ï''ô''û''ù']*   as s         { output_string out (String.lowercase s); token out lexbuf }
  | eof             					      { () }
  | _ as x                                                    { raise (Caractere_inconnu (x, Lexing.lexeme_end_p lexbuf)) }
