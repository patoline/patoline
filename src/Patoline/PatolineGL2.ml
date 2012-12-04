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

let spec = []

let files = ref []

let _ = 
  Arg.parse spec (fun x->files := x::(!files)) "Usage :";
  match !files with
    [f] ->
      let ch = open_in f in
      let pages = input_value ch in
      close_in ch;
      GL2.output pages f
  | _ ->
    Printf.fprintf stderr "%s: more than one file given!" Sys.argv.(0)



