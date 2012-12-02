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
  open Tag
  let stringOfChar = String.make 1
}


rule token acc = parse (* doit renvoyer un objet du meme type que readTreeTagger *)
  | [' ' '\t' '\n']                          	                { token acc lexbuf }
  | ['?' '.' '!']|"..." as s                                    { token ((s, SENT) :: acc) lexbuf }
  | [',' ';' ':' '\'' '/'] as s                                  { token ((stringOfChar s, PUN None) :: acc) lexbuf }
  | "ne"|"y"|"de"|"le"|"la"|"les"|"un"|"une"|"des"|"au"|"aux"
  | "du"|"l"|"en"|"n"|"d"|"à"|"c"|"s"|"se" as s                 { token ((s, PUN None) :: acc) lexbuf }
  | ['a'-'z''à''â''ç''è''é''ê''ë''î''ï''ô''û''ù']* as s         { token ((s, NAM) :: acc) lexbuf }         
  | eof             					        { List.rev acc }
  | _ as x                                                      { raise (ReadTreeTagger.Caractere_inconnu
									   (x, Lexing.lexeme_end_p lexbuf))}

