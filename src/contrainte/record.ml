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

exception End of (string list)

let ps = print_string;;
let p s = ps s; ps "/n";;

module C =
struct
  type metadata = string
  type state = string list
  let precompute _ _ s = s
  let filter _ _ = true
  let step l s = s::l
  let init_state = []
 (*pretty-printing*)
  let name = "Record";;
  let print_state = function
    |t :: s :: r :: q -> Printf.sprintf "%s::%s::%s::<%d>" t s r (List.length q)
    |[t; s]           -> Printf.sprintf "%s::%s]" t s
    |[t]              -> Printf.sprintf "%s" t
    |[]               -> "[]";;
  let print_metadata s = s;;
end

type order =
  |Add of string
  |END
      
let use_order l = function
  |Add s -> s::l
  |END   -> raise (End l)
