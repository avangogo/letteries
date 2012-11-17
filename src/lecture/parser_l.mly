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
%}

/* description des lexèmes */

%token <string> MOT
%token <string> PONCTUATION
%token FIN
%token EOF /* fin de fichier */


%start main                  /* "start" signale le point d'entrée */
%type <((string*(string list)) list) list> main     /* on _doit_ donner le type du point d'entrée */

%%
    /* --- début des règles de grammaire --- */

main:
  |ponctuation main2 FIN main {$2::$4}
  |ponctuation main2          {[$2]}
  |                           {[]}

main2:
  |MOT ponctuation main2             { ($1,$2)::($3) }
  |EOF                               { [] }


ponctuation:
  |                               {[]}
  |PONCTUATION ponctuation_option {$1::$2}


ponctuation_option:
  |                               {[]}
  |PONCTUATION ponctuation_option {$1::$2}

