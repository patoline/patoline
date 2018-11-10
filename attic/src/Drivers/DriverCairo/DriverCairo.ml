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
open RawContent
open Driver
open Color 

let driver_options = []
let filter_options argv = argv                            

let pixels_per_mm=ref 10.

let output ?(structure:structure={name="";raw_name=[];metadata=[];tags=[];
                                  page= -1;struct_x=0.;struct_y=0.;children=[||]})
    pages fileName=


  let f=try Filename.chop_extension fileName with _->fileName in
  Array.iteri (fun i x->
    let width,height=x.size in
    let widthf= (width*. !pixels_per_mm) and heightf= (height*. !pixels_per_mm) in
    let width=int_of_float widthf and height=int_of_float heightf in
    let surface = Cairo.image_surface_create Cairo.FORMAT_ARGB32 ~width:(width)
      ~height:(height)
    in
    let ctx = Cairo.create surface in

    let rec draw_page x=match x with
        Path (param,path)::s->(

          Cairo.set_line_width ctx (!pixels_per_mm*.param.lineWidth);
          List.iter (fun morceau->

            let x0,y0=morceau.(0) in
            Cairo.move_to ctx (!pixels_per_mm*.x0.(0)) (heightf-. !pixels_per_mm*.y0.(0));
            for i=0 to Array.length morceau-1 do
              let xi,yi=morceau.(i) in
              if Array.length xi=4 then (
                Cairo.curve_to ctx
                  (!pixels_per_mm*.xi.(1)) (heightf-. !pixels_per_mm*.yi.(1))
                  (!pixels_per_mm*.xi.(2)) (heightf-. !pixels_per_mm*.yi.(2))
                  (!pixels_per_mm*.xi.(3)) (heightf-. !pixels_per_mm*.yi.(3))
              ) else (
                Cairo.line_to ctx (!pixels_per_mm*.xi.(Array.length xi-1))
                  (heightf-. !pixels_per_mm*.yi.(Array.length yi-1))
              )
            done;
            if param.close then Cairo.close_path ctx;
          ) path;
          (match param.strokingColor, param.fillColor with
            | (Some cx, Some cy) ->
                let (rx,gx,bx) = to_rgb cx in
                let (ry,gy,by) = to_rgb cy in
                Cairo.set_source_rgb ctx ~red:ry ~green:gy ~blue:by;
                Cairo.fill_preserve ctx ;
                Cairo.set_source_rgb ctx ~red:rx ~green:gx ~blue:bx;
                Cairo.stroke ctx 
            | (Some cx, None   ) ->
                let (rx,gx,bx) = to_rgb cx in
                Cairo.set_source_rgb ctx ~red:rx ~green:gx ~blue:bx;
                Cairo.stroke ctx 
            | (None   , Some cy) ->
                let (ry,gy,by) = to_rgb cy in
                Cairo.set_source_rgb ctx ~red:ry ~green:gy ~blue:by;
                Cairo.fill ctx
            | (None   , None   ) -> ()
          );
          draw_page s
        )
      | Glyph g::s->(
        let out=Fonts.outlines g.glyph in
        let l=List.map (fun morceau->Array.of_list (List.map (fun (x,y)->
          (Array.map (fun xx->(xx*.g.glyph_size/.1000.+.g.glyph_x)) x,
           Array.map (fun xx->(xx*.g.glyph_size/.1000.+.g.glyph_y)) y)
        ) morceau)) out
        in
        draw_page ((Path ({default_path_param with close=true;fillColor=Some g.glyph_color;strokingColor=None},
                          l))::s)
      )
      | Link l::s->draw_page (l.link_contents@s)
      | Animation a::s ->
        draw_page (a.anim_contents.(a.anim_default) @s)
      | Dynamic d::s ->
        draw_page (d.dyn_contents ()@s)
      | (Video _|States _|Image _)::s -> draw_page s
      | Affine a::s->(
        let x0=0.
        and y0= -.heightf in
        let x1=a.affine_matrix.(0).(0)*.x0 +. a.affine_matrix.(0).(1)*.y0
        and y1=a.affine_matrix.(1).(0)*.x0 +. a.affine_matrix.(1).(1)*.y0 in
        let open Cairo in
        Cairo.transform ctx
          {xx=a.affine_matrix.(0).(0);
           yx= -.a.affine_matrix.(1).(0);
           xy= -.a.affine_matrix.(0).(1);
           yy=a.affine_matrix.(1).(1);
           x0=a.affine_matrix.(0).(2)*. !pixels_per_mm -.x1;
           y0= heightf -. a.affine_matrix.(1).(2)*. !pixels_per_mm +.y1;
          };
        draw_page a.affine_contents;
        Cairo.identity_matrix ctx;
        draw_page s)
      | []->()
    in
    draw_page (drawing_sort x.contents);
    let fname = Printf.sprintf "%s_%d.png" f i in
    Printf.fprintf stderr "Writing %s\n" fname;
    Cairo_png.surface_write_to_file surface fname;
  ) pages

let output' = output_to_prime output

let _ = 
  Hashtbl.add DynDriver.drivers "DriverCairo"
    (module struct
      let output  = output
      let output' = output'
     end:OutputDriver)
