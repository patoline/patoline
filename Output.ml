(** Output routines. An output routine is just a functor taking a driver module *)

open Constants
open Boxes
open Util
open Fonts.FTypes
open Drivers
open CamomileLibrary


let routine paragraphs (figures:drawingBox array) (opt_pages:(parameters*line) list array)=
  let draw_page p=
    let page= { pageFormat=0.,0. ; pageContents=[] } in
      List.iter (
        fun (param,line)->
          let (h,w)=page.pageFormat in
          let (h',w')=param.format in
          page.pageFormat<- (max h h', max w w');
          let y=270.0-.param.lead*.float_of_int line.height in

          let comp=compression paragraphs (param,line) in
          let rec draw_box x y comp=function
              Kerning kbox ->(
                let x'=draw_box (x+.kbox.kern_x0) (y+.kbox.kern_y0) comp kbox.kern_contents in
                  x' +. kbox.advance_width
              )
            | Hyphen h->(
                (Array.fold_left (fun x' box->
                                    let w=draw_box (x+.x') y comp box in
                                      x'+.w) 0. h.hyphen_normal)
              )
            | GlyphBox (size,a)->(
                page.pageContents<- (Drivers.Glyph { glyph_x=x;glyph_y=y; glyph_size=size;
                                                      glyph=a.Util.glyph }) :: page.pageContents;
                size*.a.width/.1000.
              )
            | Glue g->g.glue_min_width+.comp*.(g.glue_max_width-.g.glue_min_width)

            | b->box_width comp b
          in
          let rec make_line boxes x y j jmax=
            if j>=jmax then x else
              let w=draw_box x y comp boxes.(j) in
                make_line boxes (x+.w) y (j+1) jmax
          in
          let x'=(if line.hyphenStart>=0 then
                    match paragraphs.(line.paragraph).(line.lineStart) with
                        Hyphen x->(
                          let hyp=snd x.hyphenated.(line.hyphenStart) in
                          let w=make_line hyp param.left_margin y 0 (Array.length hyp) in
                            w
                        )
                      | _->param.left_margin
                  else param.left_margin)
          in
          let x''=make_line paragraphs.(line.paragraph)
            x' (270.-.param.lead*.(float_of_int line.height))
            (if line.hyphenStart>=0 then line.lineStart+1 else line.lineStart) line.lineEnd
          in
            (if line.hyphenEnd>=0 then
               match paragraphs.(line.paragraph).(line.lineEnd) with
                   Hyphen x->(
                     let hyp=fst x.hyphenated.(line.hyphenEnd) in
                       ignore (make_line hyp x'' y 0 (Array.length hyp))
                   )
                 | _->())
      ) p;
      page
  in
    Array.map draw_page opt_pages
