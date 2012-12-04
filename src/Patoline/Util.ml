(*
  Copyright Tom Hirschowitz, Florian Hatat, Pierre-Etienne Meunier,
  Christophe Raffalli and others, 2012.

  This file is part of Patoline.

  Patoline is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Patoline is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Patoline.  If not, see <http://www.gnu.org/licenses/>.
*)
exception File_not_found of (string*string list)
let findPath f path=
  let rec findPath f=function
      []->raise (File_not_found (f,path))
    | h::s ->
      if Sys.file_exists (Filename.concat h f) then
	Filename.concat h f
      else
	findPath f s
  in
    findPath f path

module Str_ = struct
  type t = string
  let compare = compare
end

module StrSet = Set.Make(Str_)
module StrMap=Map.Make(Str_)
module IntMap=Map.Make(struct type t=int let compare=compare end)
