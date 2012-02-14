(** Output routines. An output routine is just a functor taking a driver module *)

open Constants
open Boxes
open Util
open Fonts.Types
open Drivers
open CamomileLibrary

module Routine=functor (M:Driver)->struct


  let rec draw_box drv x y comp=function
      Kerning kbox ->(
        let x'=draw_box drv (x+.kbox.kern_x0) (y+.kbox.kern_y0) comp kbox.kern_contents in
          x' +. kbox.advance_width
      )
    | Hyphen h->(
        (Array.fold_left (fun x' box->
                            let w=draw_box drv (x+.x') y comp box in
                              x'+.w) 0. h.hyphen_normal)
      )
    | GlyphBox (size,a)->(
        M.text drv (x,y) size a.glyph;
        size*.a.width/.1000.
      )
    | Drawing d->(
        let x0=x-.d.drawing_x0 in
        let y0=y-.d.drawing_y0 in
          List.iter (function
                         Drawing_Box (x',y',box)->ignore (draw_box drv (x0+.x') (y0+.y') comp box)
                       | Curve (x',y',curve)->()
                    ) d.drawing_contents;
          d.drawing_x1-.d.drawing_x0
      )
    | Glue g->g.glue_min_width+.comp*.(g.glue_max_width-.g.glue_min_width)

    | b->box_width comp b


  let rec boxes_contents l i j=
    let buf=UTF8.Buf.create 256 in
      for k=i to j-1 do
        match l.(k) with
            GlyphBox (_,a)->UTF8.Buf.add_string buf (a.contents)
          | _->()
      done;
      UTF8.Buf.contents buf

  let output_routine filename paragraphs (figures:drawingBox array) (pages:(parameters*line) list array)=
    let drv=M.init (M.filename filename) in
      if Array.length pages>=1 then (

          for i=0 to Array.length pages-1 do
            match pages.(i) with
                []->()
              | (parameters,line0)::_->(
                  M.begin_page drv parameters.format;
                  let pat=[3.;3.] in
                  let wid=0.01 in
                  let make_line param line=
                    let y=270.0-.param.lead*.float_of_int line.height in
                      (* M.moveto drv (param.left_margin, 270.0-.param.lead*.float_of_int line.height); *)
                      (* M.lineto drv (param.left_margin+.param.measure,270.0-.param.lead*.float_of_int line.height); *)
                      (* M.stroke ~color:{red=0.7 ; blue=0.7 ; green=0.7}  ~dash_pattern:pat ~line_width:wid drv; *)
                      let comp=compression paragraphs (param,line) in

                      let rec make_line boxes x y j jmax=
                        if j>=jmax then x else
                          let w=draw_box drv x y comp boxes.(j) in
                            make_line boxes (x+.w) y (j+1) jmax
                      in
                      let x'=(if line.hyphenStart>=0 then
                                match paragraphs.(line.paragraph).(line.lineStart) with
                                    Hyphen x->(
                                      let hyp=snd x.hyphenated.(line.hyphenStart) in
                                      let w=make_line hyp param.left_margin y 0 (Array.length hyp) in
                                        M.end_alternative_text drv;
                                        w
                                    )
                                  | _->param.left_margin
                              else param.left_margin)
                      in
                      let x''=make_line paragraphs.(line.paragraph)
                        x' (270.-.param.lead*.(float_of_int line.height))
                        (if line.hyphenStart>=0 then line.lineStart+1 else line.lineStart) line.lineEnd
                      in
                        if line.hyphenEnd>=0 then
                          match paragraphs.(line.paragraph).(line.lineEnd) with
                              Hyphen x->(
                                let hyp=fst x.hyphenated.(line.hyphenEnd) in
                                  M.begin_alternative_text drv (boxes_contents x.hyphen_normal 0 (Array.length x.hyphen_normal));
                                  ignore (make_line hyp x'' y 0 (Array.length hyp))
                              )
                            | _->()
                  in

                  let rec make_page last_line=function
                      []->()
                    | (param,line)::s->(
                        let y=270. -. (float_of_int line.height)*.param.lead in

                          if line.isFigure then (
                            let fig=figures.(line.lastFigure) in
                            let vspace0,_=line_height paragraphs last_line in
                            let _,vspace1=line_height paragraphs line in
                            let h=fig.drawing_y1-.fig.drawing_y0 in
                            let yshift=
                              ((float_of_int (line.height - last_line.height))*.param.lead +. vspace0 -. vspace1 -.h)/.2.
                            in
                            let x0,y0,x1,y1=param.left_margin,y,
                              param.left_margin+.fig.drawing_x1-.fig.drawing_x0,
                              y+.fig.drawing_y1+.fig.drawing_y0
                            in
                              M.moveto drv (x0,y0+.yshift);
                              M.lineto drv (x0,y1+.yshift);
                              M.lineto drv (x1,y1+.yshift);
                              M.lineto drv (x1,y0+.yshift);
                              M.close_stroke drv;

                              List.iter (function
                                             Drawing_Box (x,y,l)->ignore (draw_box drv (x0+.x) (y0+.yshift+.y) 0. l)
                                           | _->()
                                        ) figures.(line.lastFigure).drawing_contents
                          ) else (
                            make_line param line;
                          );
                          make_page line s
                      )
                  in
                    make_page { paragraph=0; lineStart= -1; lineEnd= -1; hyphenStart= -1; hyphenEnd= -1; isFigure=false;
                                lastFigure=(-1); height= 0;paragraph_height= -1; page=0 } (List.rev pages.(i));
                    M.end_page drv;
                );
        done
      );
      M.close drv

end
