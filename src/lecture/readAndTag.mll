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
  open Tag
  let stringOfChar = String.make 1
  and make (s, tag) = (s, tag, s, Word.make_source ())
}


rule token acc = parse (* doit renvoyer un objet du meme type que readTreeTagger *)
  | [' ' '\t' '\n']                          	                { token acc lexbuf }
  | ['?' '.' '!']|"..." as s                                    { token ((s, Tag.tag_of_string("SENT")) :: acc) lexbuf }
  | [',' ';' ':' '\'' '/'] as s                                 { token ((stringOfChar s, Tag.tag_of_string("PUN")) :: acc) lexbuf }
  | "ne"|"y"|"de"|"le"|"la"|"les"|"un"|"une"|"des"|"au"|"aux"
  | "du"|"l"|"en"|"n"|"d"|"�"|"c"|"s"|"se"|"et" as s            { token ((s, Tag.tag_of_string("PUN")) :: acc) lexbuf }
  | ['a'-'z''�''�''�''�''�''�''�''�''�''�''�''�']* as s         { token ((s, Tag.tag_of_string("NAM")) :: acc) lexbuf }         
  | eof             					        { List.rev_map make acc }
  | _ as x                                                      { raise (ReadTreeTagger.Caractere_inconnu
									   (x, Lexing.lexeme_end_p lexbuf))}

