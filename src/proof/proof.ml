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
open Typography
open Fonts.FTypes
open Fonts
open Bezier
open OutputCommon
open OutputPaper
open Util


let _=
  let left_margin=25. in
  let right_margin=5. in
  let lead=5. in
  let bezier_au_lieu_des_glyphs=false in
  let max_line=10 in

    for i=1 to Array.length Sys.argv-1 do
      let f = Sys.argv.(i) in
      let font= loadFont f in
        Printf.printf "%d\n" (List.length (font_features font));
        (* List.iter print_subst (select_features font (font_features font)); *)
      Printf.fprintf stdout "cardinal : %d\n" (Fonts.cardinal font);
      let nb_glyphs = Fonts.cardinal font in
      let init_glyph= 0 in
      let w=
        let m=ref 0. in
          for i=0 to nb_glyphs-1 do
            let gl=loadGlyph font { empty_glyph with glyph_index=i } in
              m:=max !m (Fonts.glyphWidth gl)
          done;
          !m
      in
      let size=((210.-.left_margin-.right_margin)*.100./.w) in
      let hspace= (w*.size/.1000.) in
      let rec make_pages i0 line y p=
        let rec make_line i maxi x y0 y1 l=
          if i>=maxi || i>=nb_glyphs then l, y0, y1 else (
            let gl=loadGlyph font { empty_glyph with glyph_index=i } in
              (make_line (i+1) maxi (x+. hspace)
                 (min y0 (Fonts.glyph_y0 gl)) (max y1 (Fonts.glyph_y1 gl))
                 (
                   if not bezier_au_lieu_des_glyphs then
                     Glyph { glyph_x=x;glyph_y=0.;glyph_kx=x;glyph_ky=0.;glyph_order=0;glyph_color=black;glyph_size=size; glyph=gl }::l
                   else
                     translate x 0.
                       (resize (size/.1000.)
                          (Path ({OutputCommon.default with lineWidth=0.01},
                                 (List.map (fun a->Array.of_list a) (outlines gl))))) :: l

                 ))
          )
        in
        let l,y0,y1=make_line i0 (i0+10) 0. 0. 0. [] in
          if l=[] then [{ pageFormat=a4;
                          pageContents=p}] else (
            if line<max_line (* y -. (y1-.y0)*.size/.1000. >= bot *) then (
              let finaly= y -. y1*.size/.1000. in
                make_pages (i0+10) (line+1) (finaly +. y0*.size/.1000.-.lead ) ((Path ({OutputCommon.default with lineWidth=0.01; strokingColor=Some (RGB { red=0.8;green=0.8;blue=0.8 })},
                                                                                       [ [|[|0.;210.|],[|finaly;finaly|]|] ]))::
                                                                                  (* (Path ({Drivers.default with lineWidth=0.1}, [|[|0.;210.|],[|y;y|]|])):: *)
                                                                                  (List.map (translate left_margin (finaly)) l) @ p)
            ) else
              ({ pageFormat=a4;
                 pageContents=p})::(make_pages i0 0 280. [])
          )
      in
        Pdf.output (Array.of_list ((make_pages init_glyph 0 280. []))) ((Filename.chop_extension f)^".pdf")
    done
