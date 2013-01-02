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

open Document
open Box
open Layout
open OutputCommon
open Fonts.FTypes
open Break

let output paragraphs figures env (opt_pages:placed_line list array)=
  let draw_page i p=
    let pageContents=ref [] in
    let y0=ref infinity in
    let y0'=ref infinity in
    let y1=ref (-.infinity) in
    let x0=ref infinity in
    let x1=ref (-.infinity) in
    let top_y=ref (-.infinity) in
    List.iter (
      fun l->
        let y=l.line.height in
        top_y:=max !top_y y;
        if l.line.isFigure then (
          let fig=figures.(l.line.lastFigure) in
          y1:=max !y1 y;
          y0':=min !y0' (fig.drawing_y1+.fig.drawing_y0);
          y0:=min !y0 (fig.drawing_y1+.fig.drawing_y0);
          pageContents := (List.map (translate l.line_params.left_margin (fig.drawing_y1))
                             (fig.drawing_contents fig.drawing_nominal_width)) @ !pageContents
        ) else if l.line.paragraph<Array.length paragraphs then (
          let (yy0,yy1)=line_height paragraphs figures l.line in
          y1:=max (y+.yy1) !y1;
          y0:=min (y+.yy0) !y0;
          y0':=min (y+.yy0) !y0';
          let comp=compression paragraphs l.line_params l.line in
          let rec draw_box x y=function
          Kerning kbox ->(
            let fact=(box_size kbox.kern_contents/.1000.) in
            let w=draw_box (x+.kbox.kern_x0*.fact) (y+.kbox.kern_y0*.fact) kbox.kern_contents in
            w+.kbox.advance_width*.fact
          )
            | Hyphen h->(
              (Array.fold_left (fun x' box->
                let w=draw_box (x+.x') y box in
                x'+.w) 0. h.hyphen_normal)
            )
            | GlyphBox a->(
              pageContents:=
                (Glyph { a with glyph_x=a.glyph_x+.x;glyph_y=a.glyph_y+.y })
                           :: !pageContents;
              a.glyph_size*.Fonts.glyphWidth a.glyph/.1000.
            )
            | Glue g
            | Drawing g ->(
              let w=g.drawing_min_width+.comp*.(g.drawing_max_width-.g.drawing_min_width) in
              pageContents := (List.map (translate x y) (g.drawing_contents w)) @ !pageContents;
              w
            )
            | b->box_width comp b
          in
          x0:=min !x0 l.line_params.left_margin;
          x1:=max !x1 (fold_left_line paragraphs (fun x b->x+.draw_box x y b) l.line_params.left_margin l.line)
        )
    ) p;
    if !top_y=(-.infinity) then top_y:=0.;
    { drawing_min_width= !x1-. !x0;
      drawing_nominal_width= !x1-. !x0;
      drawing_max_width= !x1-. !x0;
      drawing_width_fixed = true;
      drawing_y0= !y0 -. !top_y;
      drawing_y1= (!y1-. !top_y);
      drawing_badness=(fun _->0.);
      drawing_contents=(fun _-> List.map (translate (-. !x0) (-. !top_y)) !pageContents) }
  in
  Array.mapi draw_page opt_pages
