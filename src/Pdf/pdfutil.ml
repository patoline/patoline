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
open Typography.Util

type obj=
    Dict of obj StrMap.t
  | Number of float
  | Name of string
  | String of string
  | Indirect of int*int
  | Array of obj list

let rec print_obj o=match o with
    Number i->Printf.printf "Number %f" i
  | Indirect (i,j)->Printf.printf "Indirect(%d,%d)" i j
  | Name s->Printf.printf "Name %s" s
  | String s->Printf.printf "String %s" s
  | Dict d->Printf.printf "Dict [";
    StrMap.iter (fun k a->Printf.printf "(%S, " k;print_obj a;Printf.printf ")") d;
    Printf.printf "]"
  | Array a->Printf.printf "Array [";
    List.iter (fun a->Printf.printf "(";print_obj a;Printf.printf ")") a;
    Printf.printf "]"
