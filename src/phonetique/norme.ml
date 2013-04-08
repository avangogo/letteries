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

let filtre =
  let res = String.make 256 ' '(*'*'*) in
  for i = 97 to 122 do 
    res.[i]    <- char_of_int i; (*minuscules*)
    res.[i-32] <- char_of_int i (*majuscules*)
  done;
  let enregistre c liste =
    List.iter (fun x -> res.[int_of_char x] <- c) liste in
  enregistre 'a' ['�';'�';'�';'�';'�';'�';'�';'�';'�';'�';'�';'�'];
  enregistre '�' ['�';'�';'�';'�';'�';'�'];
  enregistre '�' ['�';'�'];
  enregistre 'i' ['�';'�';'�';'�';'�';'�';'�';'�'];
  enregistre 'o' ['�';'�';'�';'�';'�';'�';'�';'�';'�';'�'];
  enregistre 'u' ['�';'�';'�';'�';'�';'�';'�';'�'];
  enregistre 'y' ['�';'�';'�'];
  enregistre ' ' [' '; '!'; '"'; '\''; '(' ; ')'; ','; '-'; '.'; ':'; ';'; '?'; '\n'];
  res;;

let formate s =
  let n = String.length s in
  let res = String.copy s in
  for i = 0 to n-1 do
    res.[i] <- filtre.[int_of_char s.[i]]
  done;
  res;;

let segmente s0 =
  let s = formate s0 in
  let n = String.length s in
  let j = ref 0 in
  let res = ref [] in
  for i = 0 to n-1 do
    if (s.[i] = ' ') then
      begin	
        if (i <> !j) then res := (String.sub s (!j) (i - !j))::(!res);
        j   := i+1
      end
  done;
  if (n <> !j) then res := (String.sub s (!j) (n - !j))::(!res);
  List.rev (!res);;

(* let toto = " a � �� de notr�� ��� e bonheur,   toi, le fatal embl�me !
Salut de ABCDE la d�mence et libation bl�me  ";;
	
print_string "toto:";;
print_newline ();;
List.iter
(fun x -> print_string x; print_newline ())
(segmente
toto) *)
