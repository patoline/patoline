(*
  Copyright Florian Hatat, Tom Hirschowitz, Pierre Hyvernat,
  Pierre-Etienne Meunier, Christophe Raffalli, Guillaume Theyssier 2012.

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

exception No_matching_path of (string*string list)

(** [findpath fname paths] tries to find the file [fname] in any directory
    given in the list [paths]. If it succeds, the full path to [fname] is
    returned, otherwise [No_matching_path] is raised. *)
let findPath fname paths =
  let rec findPath fname = function
    | []    -> raise (No_matching_path (fname ,paths))
    | p::ps -> let path = Filename.concat p fname in
               if Sys.file_exists path
               then path
               else findPath fname ps
  in findPath fname paths

(** [chop_extension' fname] is the same as [Filename.chop_extension fname] but
    if [fname] does not have an extension, [fname] is returned instead of
    raising [Invalid_argument]. *)
let chop_extension' fname =
  try Filename.chop_extension fname
  with _ -> fname

(** [get_extension fname] returns the extension of the file [fname]. If the
    file does not have an extension, [Invalid_argument] is raised. *)
let get_extension fname =
  let baselen = String.length (chop_extension' fname) in
  let extlen  = String.length fname - baselen - 1 in
  if extlen <= 0
  then let err = Printf.sprintf "No extension in filename '%s'." fname in
       raise (Invalid_argument err)
  else String.sub fname (baselen + 1) extlen

(** [get_extension' fname] is the same as [get_extension fname] but if [fname]
    does not have an extension, the empty string is returned and no exception
    is raised. *)
let get_extension' fname =
  try get_extension fname
  with _ -> ""
