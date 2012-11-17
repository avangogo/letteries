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
  open Parser;;        (* le type "token" est défini dans parser.mli *)
  exception Eof;;
  exception Caractere_inconnu of char;;
}


rule token = parse    (* la "fonction" aussi s'appelle token .. *)
  | [' ' '\t' '\n']                           { token lexbuf }    (* on saute les blancs *)
  | "(*" [^ '*']* "*)"                        { token lexbuf } (*pas d'étoile dans un commentaire*)
  | eof             	               	      { EOF }
  | "="                                       { EGAL }
  | "->"                                      { FLECHE }
  | "*section*"                               { SEPARATEUR }
  | '['                                       { LCROCHET }
  | ']'                                       { RCROCHET }
  | '\''                                      { APOSTROPHE }
  | '('                                       { LPAREN }
  | ')'                                       { RPAREN }
  | "FIN"                                     { FIN }
  | ['a'-'z' '^' '~' '\233' '\232'] as x      { LETTRE x }
  | ['A'-'Z'] as x                            { VARIABLE x }
  | _ as x                                                    { raise (Caractere_inconnu x)}
