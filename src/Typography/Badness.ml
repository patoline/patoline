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
open Box
open Layout

let v_badness v_space haut max_haut params_i comp_i bas max_bas params_j comp_j=

  let xi=ref params_i.left_margin in
  let xj=ref params_j.left_margin in

  let rec collide i j w_tot col col2=
    let box_i=if i<max_haut then haut.(i) else Empty in
    let box_j=if j<max_bas then bas.(j) else Empty in
      (* let _=Graphics.wait_next_event [Graphics.Key_pressed] in *)
    let wi=box_width comp_i box_i in
    let wj=box_width comp_j box_j in
      if !xi +.wi < !xj+. wj && i < max_haut then (
        let yi=lower_y box_i in
        let yj=if !xi+.wi < !xj then -.infinity else
          if upper_y box_j > -.infinity then upper_y box_j else 0.
        in
        let x0=if !xi+.wi < !xj then !xi else max !xi !xj in
        let w0= !xi +. wi -. x0 in

          xi:= !xi+.wi;
          if !xj>= !xi+.wi || is_glue box_i || is_glue box_j then
            (collide (i+1) j w_tot col col2)
          else
            (let area=w0*.(v_space+.yi-.yj) in
             let cl=classify_float area in
             let ar=if cl=FP_infinite || cl=FP_nan then 0. else area in
             collide (i+1) j (w_tot+.w0) (col+.ar) (col+.ar*.ar))
      ) else if j < max_bas then (
        let yi=if !xj +. wj < !xi then infinity else
          if lower_y box_i < infinity then lower_y box_i else 0. in
        let yj=upper_y box_j in
        let x0=if !xj+.wj < !xi then !xj else max !xi !xj in
        let w0= !xj +. wj -. x0 in
          xj:= !xj+.wj;
          if !xi>= !xj+.wj || is_glue box_i || is_glue box_j then
            (collide i (j+1) w_tot col col2)
          else
            (let area=w0*.(v_space+.yi-.yj) in
             let cl=classify_float area in
             let ar=if cl=FP_infinite || cl=FP_nan then 0. else area in
             collide i (j+1) (w_tot+.w0) (col+.ar) (col+.ar*.ar))
      ) else (if w_tot<=0. then 0. else ((col*.col-.col2)/.w_tot))
  in
    collide 0 0 0. 0. 0.




let h_badness paragraphs measure node comp=
  let bad=ref 0. in
  let glues=ref 0 in
  for k=node.lineStart to node.lineEnd-1 do
    match paragraphs.(node.paragraph).(k) with
        Drawing x ->
          bad:= !bad +.
            x.drawing_badness (x.drawing_min_width+.(x.drawing_max_width-.x.drawing_min_width)*.comp)
      | Glue x->(
        bad:= !bad +.
          x.drawing_badness (x.drawing_min_width+.(x.drawing_max_width-.x.drawing_min_width)*.comp);
        incr glues
      )
      | _->()
  done;
  if !glues<=0 then 10e6 else
    !bad *. (if node.nom_width/.measure < 2./.9. || node.nom_width/.measure > 7./.9. then 20. else 1.)
