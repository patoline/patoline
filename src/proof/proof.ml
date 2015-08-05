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
open FTypes
open Fonts
open RawContent
open Color
open Driver

(* Configuration *)
let margin      = 10.0
let use_bezier  = false
let nb_lines    = 10
let nb_columns  = 10
let page_format = Util.a4

let build_font_file file =
  let font = loadFont file in
  let ofile = Filename.chop_extension file ^ ".pdf" in
  let nb_features = List.length (font_features font) in
  let nb_glyphs = Fonts.cardinal font in
  Printf.printf "File: %s\n%!" file;
  Printf.printf "  %d features\n%!" nb_features;
  Printf.printf "  %d glyphs\n%!" nb_glyphs;

  let get_glyph i = loadGlyph font { empty_glyph with glyph_index = i } in
  let glyphs = Array.init nb_glyphs get_glyph in

  let glyphHeight gl = glyph_y1 gl -. glyph_y0 gl in
  let maxsz (mw,mh) gl = (max mw (glyphWidth gl), max mh (glyphHeight gl)) in
  let (maxw,maxh) = Array.fold_left maxsz (0.0,0.0) glyphs in

  let size = (fst page_format -. 2.0 *. margin) /. float_of_int nb_columns *. 100.0 in
  let hspace = maxw *. size /. 1000.0 in
  let rec make_pages i0 line y p=
    let rec make_line i maxi x y0 y1 l=
      if i>=maxi || i>=nb_glyphs then l, y0, y1 else (
        let gl = glyphs.(i) in
          (make_line (i+1) maxi (x+. hspace)
             (min y0 (Fonts.glyph_y0 gl)) (max y1 (Fonts.glyph_y1 gl))
             (
               if not use_bezier then
                 Glyph { glyph_x=x;glyph_y=0.;glyph_kx=x;glyph_ky=0.;glyph_order=0;glyph_color=black;glyph_size=size; glyph=gl }::l
               else
                 translate x 0.
                   (resize (size/.1000.)
                      (Path ({default_path_param with lineWidth=0.01},
                             (List.map (fun a->Array.of_list a) (outlines gl))))) :: l
             ))
      )
    in
    let l,y0,y1=make_line i0 (i0+10) 0. 0. 0. [] in
      if l=[] then [{ size = page_format; contents=p}] else (
        if line<nb_lines (* y -. (y1-.y0)*.size/.1000. >= bot *) then (
          let finaly= y -. y1*.size/.1000. in
            make_pages (i0+10) (line+1) (finaly +. y0*.size/.1000.-.margin )
              ((Path ({default_path_param with lineWidth=0.01; strokingColor=Some (rgb 0.8 0.8 0.8)},
               [ [|[|0.;210.|],[|finaly;finaly|]|] ]))::
               (* (Path ({Drivers.default with lineWidth=0.1}, [|[|0.;210.|],[|y;y|]|])):: *)
               (List.map (translate margin (finaly)) l) @ p)
        ) else
          ({ size=page_format; contents=p})::(make_pages i0 0 280. [])
      )
  in
  Pdf.output (Array.of_list (make_pages 0 0 280. [])) ofile

let _ =
  for i = 1 to Array.length Sys.argv - 1 do
    build_font_file Sys.argv.(i)
  done
