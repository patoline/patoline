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
open Drivers
open Maths
open Util

let a =
  let env=Maths.default in
  let style=Display in
    Ordinary  (noad (glyphs "a"))

let a0=
  let env=Maths.default in
  let style=Display in
    Ordinary  { (noad (glyphs "a")) with
                  superscript_right=[Ordinary (noad (glyphs "0"))];
                  superscript_left=[];
                  subscript_right=[Ordinary (noad (glyphs "1"))];
                  subscript_left=[] }


let bx= (
  Binary { bin_priority=0; bin_drawing=Normal (false, (noad (glyphs "+")), false);
           bin_left=[a]; bin_right=[a] }
)


let x =
  let env=Maths.default in
  let style=Display in
    Ordinary  { (noad (glyphs "V")) with
                  superscript_right=[];
                  superscript_left=[];
                  subscript_right=[Ordinary (noad (glyphs "i"))];
                  subscript_left=[] }

let integrale=
  let env=Maths.default in
  let style=Display in
    Operator { op_limits=false; op_noad={ (noad [int env style]) with
                                            superscript_right=[Ordinary (noad (glyphs "0"))];
                                            superscript_left=[];
                                            subscript_right=[Ordinary (noad (glyphs "a=0+1"))];
                                            subscript_left=[]
                                        };
               op_left_spacing=0.5;
               op_right_spacing=0.4;
               op_left_contents=[a];
               op_right_contents=[a] }

let cos=
  let env=Maths.default in
  let style=Display in
    Ordinary  { (noad (gl_font env style (Fonts.loadFont "Otf/lmromancaps10-regular.otf") "cos")) with
                  superscript_right=[];
                  superscript_left=[];
                  subscript_right=[];
                  subscript_left=[] }

let u=
  (* let (a,b,c,d)=bounding_box bx in *)

  let env=Maths.default in
  let st=Display in
    [| { pageFormat=(100.,100.); pageContents=
           List.map (translate 20. 20.) (
             List.map (Drivers.resize 10.) (
               draw_boxes (
                 (draw_maths env st [cos])
                 @(draw_maths env st [Decoration (open_close (gl env st "(") (gl env st ")"), [a])])
                 @(draw_maths env st [x]))
             ))
       }
    |]

let _=
  Drivers.Pdf.output u "maths.pdf"
