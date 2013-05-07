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
  open Tag;;
}

let word = ['\'''.' ',' ';' '?' ':' '\'' '/' '!''a'-'z''à''â''ç''è''é''ê''ë''î''ï''ô''û''ù']*
let letters = ['a'-'z' 'A'-'Z' ':'] 
let tag = letters* (':' letters*)?

rule token acc = parse
  | ((word as word) '\t')                             { let tag = tag lexbuf in
							newline lexbuf;
							(* Traduction en même temps *)
						        token ((UseCamomile.utf8_of_latin0 word, tag) :: acc) lexbuf }
  | eof             		        	      { List.rev acc }
  | _ as x                                            { raise (Caractere_inconnu (x, Lexing.lexeme_end_p lexbuf)) }

and newline = parse
  | '\n'        {()}

and tag = parse
  | "ABR"	{ABR}
  | "ADJ"	{ADJ}
  | "ADV"	{ADV}
  | "DET:ART"	{DET ART}
  | "DET:POS"	{DET Pos}
  | "INT"	{INT}
  | "KON"	{KON}
  | "NAM"	{NAM}
  | "NOM"	{NOM}
  | "NUM"	{NUM}
  | "PRO"	{PRO None}
  | "PRO:DEM"	{PRO (Some DEM)}
  | "PRO:IND"	{PRO (Some IND)}
  | "PRO:PER"	{PRO (Some PER)}
  | "PRO:POS"	{PRO (Some POS)}
  | "PRO:REL"	{PRO (Some REL)}
  | "PRP"	{PRP None}
  | "PRP:det"	{PRP (Some Det)}
  | "PUN"	{PUN None}
  | "PUN:cit"	{PUN (Some Cit)}
  | "SENT"	{SENT}
  | "SYM"	{SYM}
  | "VER:cond"  {VER Cond}
  | "VER:futu"  {VER Futu}	
  | "VER:impe"	{VER Impe}
  | "VER:impf"	{VER Impf}
  | "VER:infi"	{VER Infi}
  | "VER:pper"	{VER Pper}
  | "VER:ppre"	{VER Ppre}
  | "VER:pres"	{VER Pres}
  | "VER:simp"	{VER Simp}
  | "VER:subi"	{VER Subi}
  | "VER:subp"	{VER Subp}
  | _ as x                                            { raise (Caractere_inconnu (x, Lexing.lexeme_end_p lexbuf)) }
