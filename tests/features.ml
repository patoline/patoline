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
open Fonts.Opentype
open Fonts.FTypes

let f=loadFont "AGaramondPro-Regular.otf"

let _=
  List.iter print_subst (select_features f [SmallCapitals]);
  let chars=['T';'i';'t';'r''e'] in
  let nums=List.map (fun x->{ empty_glyph with glyph_index=glyph_of_char f) chars in
    List.iter (Printf.printf "%d\n") glyphs;
    List.fold_left apply nums (select_features f [SmallCapitals])
