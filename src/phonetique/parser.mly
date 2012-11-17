/*  Lettreries is a random poem generator.
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
    along with this program.  If not, see <http://www.gnu.org/licenses/>. */

%{
(* --- préambule: ici du code Caml --- *)
  open Regles;;
  (*open Phoneme;;*)
  let string_of_charlist l =
    let n = List.length l in
    let s = String.make n '*' in
    let _ = List.fold_left (fun i c -> s.[i] <- c; i+1) 0 l in
      s;;
%}

/* description des lexèmes */

%token EOF
%token EGAL
%token FLECHE
%token LCROCHET RCROCHET
%token APOSTROPHE
%token LPAREN RPAREN
%token FIN
%token <char> LETTRE
%token <char> VARIABLE
%token SEPARATEUR

%start main                  /* "start" signale le point d'entrée */
%type <((char*(Regles.lettre0 list))list)*(Regles.regle0 list)*((string*(Phoneme.phoneme list)) list)> main
/* on _doit_ donner le type du point d'entrée */

%%
    /* --- début des règles de grammaire --- */

/* --- définitions, regles, exceptions --- */
main:
  |definitions SEPARATEUR regles SEPARATEUR exceptions EOF         {($1,$3,$5)}

/* ************* définitions ************* */
definitions:
  |VARIABLE EGAL LCROCHET lettres RCROCHET definitions { ($1,$4)::$6 }
  |   { [] }

/* ************* regles ************* */
regles:
  |regle regles {$1::$2}
  |             {[]}

regle:
  |contexte lettre lettres contexte FLECHE phonetique          {Regles.Fleche0($1,$2::$3,$4,$6)}

contexte:
  |LCROCHET lettres RCROCHET                   {$2}
  |                                            {[]}
  |contexte FIN                                {$1@[Regles.Fin0]}

lettres:
  |lettre lettres {$1::$2} 
  | {[]}

lettre:
  |LETTRE   {Regles.Lettre0 $1} 
  |VARIABLE {Regles.Var0 $1}

/* exceptions */

exceptions:
  |exption exceptions {$1::$2}
  |                   {[]}

exption:
  | chars FLECHE phonetique  {(string_of_charlist $1),$3}

phonetique:
  |phonetiques LPAREN muet RPAREN {$1@$3}
  |phonetiques {$1}

phonetiques:
  |APOSTROPHE chars APOSTROPHE phonetiques  {(Phoneme.of_string(string_of_charlist $2))::$4}
  |{[]}

muet:
  |APOSTROPHE LETTRE APOSTROPHE  muet {(Phoneme.of_string0(string_of_charlist [$2]))::$4}
  | {[]}

chars:
  |LETTRE chars {$1 :: $2}
  |{[]}
