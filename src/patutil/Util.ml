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

open Extra

(** [pt_of_mm l] converts [l] from Adobe points to millimeters. *)
let pt_of_mm : float -> float = fun x -> (72.0 *. x) /. 25.4

(** [mm_of_pt l] converts [l] from millimeters to Adobe points. *)
let mm_of_pt : float -> float = fun x -> (25.4 *. x) /. 72.0

(** Width and height of the A4 page format (in millimeters). *)
let a4 : float * float = (210.0, 297.0)

(* a lighter split that calling str *)
let split char str =
  let len = String.length str in
  let rec fn beg pos acc =
    if pos >= len then List.rev (String.sub str beg (pos - beg)::acc)
    else if str.[pos] = char then fn (pos+1) (pos+1)  (String.sub str beg (pos - beg)::acc)
    else fn beg (pos+1) acc
  in
  fn 0 0 []

(* A type needed both by Db and RawContent *)
type visibility = Private | Group | Public
let vis_to_string = function
  | Private -> "Private"
  | Group -> "Group"
  | Public -> "Public"
