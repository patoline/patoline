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
open Typography.OutputCommon
open Typography.OutputPaper
open Typography.Util

let output ?(structure:structure={name="";displayname=[];metadata=[];tags=[];
				  page= -1;struct_x=0.;struct_y=0.;substructures=[||]})
    pages fileName=


  let factor=10. in
  let f=try Filename.chop_extension fileName with _->fileName in
  Array.iteri (fun i x->
    let width,height=x.pageFormat in
    let widthf= (width*.factor) and heightf= (height*.factor) in
    let width=int_of_float widthf and height=int_of_float heightf in
    let surface = Cairo.image_surface_create Cairo.FORMAT_ARGB32 ~width ~height in
    let ctx = Cairo.create surface in

    let rec draw_page x=match x with
        Path (param,path)::s->(

          Cairo.set_line_width ctx (factor*.param.lineWidth);
          List.iter (fun morceau->

            let x0,y0=morceau.(0) in
            Cairo.move_to ctx (factor*.x0.(0)) (heightf-.factor*.y0.(0));
            for i=0 to Array.length morceau-1 do
              let xi,yi=morceau.(i) in
              if Array.length xi=4 then (
                Cairo.curve_to ctx
                  (factor*.xi.(1)) (heightf-.factor*.yi.(1))
                  (factor*.xi.(2)) (heightf-.factor*.yi.(2))
                  (factor*.xi.(3)) (heightf-.factor*.yi.(3))
              ) else (
                Cairo.line_to ctx (factor*.xi.(Array.length xi-1))
                  (heightf-.factor*.yi.(Array.length yi-1))
              )
            done;
            if param.close then Cairo.close_path ctx;
          ) path;
          (match param.strokingColor,param.fillColor with
              Some (RGB x),Some (RGB y)->(
                Cairo.set_source_rgb ctx ~red:y.red ~green:y.green ~blue:y.blue;
                Cairo.fill_preserve ctx ;
                Cairo.set_source_rgb ctx ~red:x.red ~green:x.green ~blue:x.blue;
                Cairo.stroke ctx ;
              )
            | Some (RGB x),None->(
              Cairo.set_source_rgb ctx ~red:x.red ~green:x.green ~blue:x.blue;
              Cairo.stroke ctx ;
            )
            | None,Some (RGB y)->(
              Cairo.set_source_rgb ctx ~red:y.red ~green:y.green ~blue:y.blue;
              Cairo.fill ctx ;
            )
            | _->()
          );
          draw_page s
        )
      | Glyph g::s->(
        let out=Typography.Fonts.outlines g.glyph in
        let l=List.map (fun morceau->Array.of_list (List.map (fun (x,y)->
          (Array.map (fun xx->(xx*.g.glyph_size/.1000.+.g.glyph_x)) x,
           Array.map (fun xx->(xx*.g.glyph_size/.1000.+.g.glyph_y)) y)
        ) morceau)) out
        in
        draw_page ((Path ({default with close=true;fillColor=Some g.glyph_color;strokingColor=None},
                          l))::s)
      )
      | Link l::s->draw_page (l.link_contents@s)
      | _::s->(draw_page s)
      | []->()
    in
    draw_page (drawing_sort x.pageContents);
    Cairo_png.surface_write_to_file surface (Printf.sprintf "%s%d.png" f i);
  ) pages

(* open Typography.Fonts *)
(* open Typography.Fonts.FTypes *)
(* let _= *)
(*   let dr=[Path({default with fillColor=Some red;strokingColor=Some green},[rectangle (100.,100.) (200.,200.)])] in *)
(*   let font=loadFont "/Users/pe/Projets/patoline/Fonts/Alegreya/Alegreya-Regular.otf" in *)
(*   let gl=loadGlyph font { glyph_utf8="A";glyph_index=(glyph_of_char font 'A')} in *)
(*   let g=Glyph { glyph_x=10.;glyph_y=10.;glyph=gl;glyph_color=black;glyph_size=3.8} in *)
(*   output [|{pageFormat=a4;pageContents=[g]}|] "test" *)

let output'=output_to_prime output

open Typography.Box
open Typography.Document
let makeImage filename cont env=
  let w=cont.drawing_nominal_width in
  let h=cont.drawing_y1-.cont.drawing_y0 in
  output [|{pageFormat=(w,h);pageContents=List.map (translate 0. (-.cont.drawing_y0)) (cont.drawing_contents w)}|] filename;
  let f=try Filename.chop_extension filename with _->filename in

  let i={image_file=(Printf.sprintf "%s0.png" f);
         image_width=w;
         image_height=h;
         image_x=0.;
         image_y=cont.drawing_y0;
         image_order=0;
        }
  in
  let img={
    drawing_min_width=w;
    drawing_max_width=w;
    drawing_nominal_width=w;
    drawing_y0=cont.drawing_y0;
    drawing_y1=cont.drawing_y1;
    drawing_badness=(fun _->0.);
    drawing_contents=(fun _->[Typography.OutputCommon.Image i])
  }
  in
  img
