(*  Lettreries is a random poem generator.
    Copyright (C) 2012 R�mi de Verclos

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
   exception Caractere_non_valide of char*string;;
}


rule token out = parse
  | ['\n' '\r']                                               { Lexing.new_line lexbuf; output_char out '\n'; token out lexbuf }
  | [' ' '\t' ] as c                             	      { output_char out c; token out lexbuf }
  | "(*" [^ '*']* "*)"
  | "(*" [^ ')']* "*)"		                              { token out lexbuf } (* pas d'�toile dans un commentaire (lex est mauvais..) *)
  | ['(' ')']                                                 { token out lexbuf }      
  | ".." ['.']*                                               { output_string out "..." ; token out lexbuf}
  | "'"|"`"|"\'"                                              { output_string out "'" ; token out lexbuf}
  | "�"|"�"|'"'                                               { token out lexbuf }
  | "-"                                                       { output_char out ' '; token out lexbuf } (*a am�liorer: il faudrait garder ceux des mots compos�s et des formes verbales invers�es "est-il"*)
  | ['.' ',' ';' '?' ':' '\'' '/' '!' ] as s                  { output_char out s; token out lexbuf}
  | ['A'-'Z' 'a'-'z'
	'�''�''�''�''�''�''�''�''�''�''�''�'
        '�''�''�''�''�''�''�''�''�''�''�''�']*   as s         { output_string out (String.lowercase s); token out lexbuf }
  | eof             					      { () }
  | _ as x                                                    { let pos = lexbuf.Lexing.lex_curr_p in
								raise (Caractere_non_valide (x, 
									Printf.sprintf "Line %d, Column %d"
										pos.Lexing.pos_lnum
										(pos.Lexing.pos_cnum - pos.Lexing.pos_bol))) }









