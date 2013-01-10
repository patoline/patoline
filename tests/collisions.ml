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
open Drivers
open Binary
open Typography
open Constants
open Lexing
open Util
open CamomileLibrary
open Fonts.FTypes

let spec = []

exception Syntax_Error of Lexing.position * string

let default=
  { lead=5.;
    measure=150.;
    lines_by_page=45;
    left_margin=(fst a4-.150.)/.2.;
    local_optimization=0;
    min_page_diff=0;
    min_height_before=0;
    min_height_after=0;
  }



let test=
  "Ceci est un petit texte sympa, avec un premier paragraphe assez long "^
  "pour avoir des sauts de lignes : avec même des accents, surtout l'été !"

let mm=8.


let draw_glyph (x0,y0) (size,gl)=
  let fact=mm*.size/.1000. in
  List.iter (fun (x,y)->
               if Array.length x = 2 then
                 (let (a,b)=x.(0),y.(0) in
                  let (c,d)=x.(1),y.(1) in
                    Graphics.moveto (x0+round (a*.fact)) (y0+round (b*.fact));
                    Graphics.lineto (x0+round (c*.fact)) (y0+round (d*.fact)))
               else
                 (let (a,b)=x.(0),y.(0) in
                  let (c,d)=x.(1),y.(1) in
                  let (e,f)=x.(2),y.(2) in
                  let (g,h)=x.(3),y.(3) in
                    Graphics.moveto (x0+round (a*.fact)) (y0+round (b*.fact));
                    Graphics.curveto (x0+round (c*.fact), y0+round (d*.fact))
                      (x0+round (e*.fact), y0+round (f*.fact)) (x0+round (g*.fact), y0+round (h*.fact)))
            ) (List.concat (Fonts.outlines gl))

let draw_line y0 text comp=
    ignore (Array.fold_left (fun x box->
                               let rec draw_box xx=function
                                   GlyphBox g -> draw_glyph (int_of_float xx,y0) (g.glyph_size,g.glyph)
                                 | Kerning g -> draw_box (xx+.g.kern_x0) g.kern_contents
                                 | _->()
                               in
                                 draw_box x box;
                                 x +. (box_width comp box)*.mm) 0. text)


let comp_i=1.
let comp_j=0.5
let xi=ref 0.
let xj=ref 0.

let yi0=200
let yj0=100

let rec collide boxes_i i maxi boxes_j j maxj max_col=
  let box_i=if i>=maxi then Empty else boxes_i.(i) in
  let box_j=if j>=maxj then Empty else boxes_j.(j) in
  let _=Graphics.wait_next_event [Graphics.Key_pressed] in
  let wi=box_width comp_i box_i in
  let wj=box_width comp_j box_j in
    if !xi +.wi < !xj+. wj && i < Array.length boxes_i then (

      let yi_=lower_y box_i wi in
      let yi=if yi_=infinity then 0. else yi_ in
      let yj_=if !xi+.wi < !xj then 0. else upper_y box_j wj in
      let yj=if yj_=(-.infinity) then 0. else yj_ in
      let x0=if !xi+.wi < !xj then !xi else max !xi !xj in
      let w0= !xi +. wi -. x0 in

        Graphics.draw_rect (round (mm*. x0)) (yj0 + round (mm*. yj))
          (round (mm*. (w0))) (yi0 -yj0 + round (mm*. (yi-.yj)));

        xi:= !xi+.wi;


        collide boxes_i (i+1) boxes_j j (min max_col (yi-.yj))
    ) else if j < Array.length boxes_j then (
      let yi_=if !xj > !xi +. wi then 0. else lower_y box_i wi in
      let yi=if yi_=(infinity) then 0. else yi_ in
      let yj_=upper_y box_j wj in
      let yj=if yj_=(-.infinity) then 0. else yj_ in

      let x0=if !xj+.wj < !xi then !xj else max !xi !xj in
      let w0= !xj +. wj -. x0 in
        Graphics.draw_rect (round (mm*. x0)) (yj0 + round (mm*. yj))
          (round (mm*. w0)) (yi0 -yj0 + round (mm*. (yi-.yj)));
        xj:= !xj+.wj;
        collide boxes_i i boxes_j (j+1) (min max_col (yi-.yj))
    )



let _=
  let text=Array.of_list (boxify Typography.defaultEnv [T test]) in

    Graphics.open_graph "";
    draw_line yi0 text comp_i;
    draw_line yj0 text comp_j;
    collide text 0 text 0 0.;
    let _=Graphics.wait_next_event [Graphics.Key_pressed] in ()
