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

val build :
  (string -> bool -> Tag.tag -> string -> 'meta) ->
  (string * (State.wild * State.wild list) list) list ->
  (State.t * (State.t * 'meta list) list) list


module type Bdd  =
  functor (C : Contrainte.Constraint) ->
sig
  type t
  type trans
  val build : (string * (State.wild * State.wild list) list) list -> t
  val get : t -> State.t -> trans
  val choose : trans -> (State.t * C.metadata list) * trans
end

module ListBdd : Bdd
