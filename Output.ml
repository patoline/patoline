(** Output routines. An output routine is just a functor taking a driver module *)

open Constants
open Boxes
open Util
open FontsTypes
open Drivers
module Routine=functor (M:Driver)->struct

  let output_routine filename paragraphs (pages:(parameters*line) list array)=
    let drv=M.init (M.filename filename) in
      if Array.length pages>=1 then (
        for i=0 to Array.length pages-1 do
          match pages.(i) with
              []->()
            | (parameters,_)::_->(
                M.begin_page drv parameters.format;
                let pat=[3.;3.] in
                let wid=0.01 in
                  (* M.moveto drv (x0 , 270.0); *)
                  (* M.lineto drv (x0 , 270.0 -. parameters.lead *. (float_of_int parameters.lines_by_page-.1.)); *)
                  (* M.moveto drv (x0 +. w , 270.0 -. parameters.lead *. (float_of_int parameters.lines_by_page-.1.)); *)
                  (* M.lineto drv (x0 +. w , 270.0); *)

                  List.iter (fun (param,line)->
                               M.moveto drv (param.left_margin, 270.0-.param.lead*.float_of_int line.height);
                               M.lineto drv (param.left_margin+.param.measure,270.0-.param.lead*.float_of_int line.height);
                               M.stroke ~color:{red=0.7 ; blue=0.7 ; green=0.7}  ~dash_pattern:pat ~line_width:wid drv;
                               let comp=compression paragraphs (param,line) in
                               let rec make_line boxes x y j jmax=
                                 if j>=jmax then x else
                                   match boxes.(j) with
                                       Glue g->(
                                         let w=g.glue_min_width+.comp*.(g.glue_max_width-.g.glue_min_width) in
                                           make_line boxes (x+.w) y (j+1) jmax
                                       )
                                     | Kerning kbox ->(
                                         let _=make_line [|kbox.kern_contents|] (x+.kbox.kern_x0) (y+.kbox.kern_y0) 0 1 in
                                           make_line boxes (x+.kbox.advance_width) y (j+1) jmax
                                       )
                                     | Hyphen h->(
                                         let x'=make_line h.hyphen_normal x y 0 (Array.length h.hyphen_normal) in
                                           make_line boxes x' y (j+1) jmax
                                       )
                                     | GlyphBox (size,a)->(
                                         M.text drv (x,y) size [a.glyph];
                                         make_line boxes (x +. box_width comp boxes.(j)) y (j+1) jmax
                                       )
                                     | box->make_line boxes (x+.(box_width comp box)) y (j+1) jmax
                               in
                               let y=270. -. (float_of_int line.height)*.param.lead in
                               let x0=(if line.hyphenStart>=0 then
                                         match paragraphs.(line.paragraph).(line.lineStart-1) with
                                             Hyphen x->let hyp=snd x.hyphenated.(line.hyphenStart) in
                                               make_line hyp param.left_margin y 0 (Array.length hyp)
                                           | _->param.left_margin
                                       else param.left_margin)
                               in
                               let x1=make_line paragraphs.(line.paragraph)
                                 x0 (270.-.param.lead*.(float_of_int line.height))
                                 line.lineStart line.lineEnd
                               in
                                 if line.hyphenEnd>=0 then
                                   match paragraphs.(line.paragraph).(line.lineEnd) with
                                       Hyphen x->let hyp=fst x.hyphenated.(line.hyphenEnd) in
                                         ignore (make_line hyp x1 y 0 (Array.length hyp))
                                     | _->()
                            ) pages.(i);
                  M.end_page drv;
              )
        done
      );
      M.close drv

end
