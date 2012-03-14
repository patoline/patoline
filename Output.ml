(** Output routines. An output routine is just a functor taking a driver module *)

open Constants
open Typeset
open Util
open Fonts.FTypes
open Drivers
open CamomileLibrary
open Typography

let routine paragraphs (figures:drawingBox array) env (opt_pages:(parameters*line) list array)=
  let positions=Array.make (Array.length paragraphs) (0,0.,0.) in
  let par=ref (-1) in
  let draw_page i p=
    let page= { pageFormat=a4 ; pageContents=[] } in
      List.iter (
        fun (param,line)->
          let y=270.0-.param.lead*.float_of_int line.height in

            if line.isFigure then (

              let fig=figures.(line.lastFigure) in
              page.pageContents<- (List.map (translate param.left_margin y) (fig.drawing_contents fig.drawing_nominal_width)) @ page.pageContents;


            ) else (

              if line.paragraph<> !par then (
                par:=line.paragraph;
                positions.(!par)<-(i,0., y+. param.lead/.2. +. snd (line_height paragraphs line))
              );

              let comp=compression paragraphs (param,line) in
              let rec draw_box x y comp=function
                  Kerning kbox ->(
                    let w=draw_box (x+.kbox.kern_x0) (y+.kbox.kern_y0) comp kbox.kern_contents in
                      w+.kbox.advance_width
                  )
                | Hyphen h->(
                    (Array.fold_left (fun x' box->
                                        let w=draw_box (x+.x') y comp box in
                                          x'+.w) 0. h.hyphen_normal)
                  )
                | GlyphBox a->(
                    page.pageContents<- (Drivers.Glyph { a with glyph_x=a.glyph_x+.x;glyph_y=a.glyph_y+.y }) :: page.pageContents;
                    a.glyph_size*.Fonts.glyphWidth a.glyph/.1000.
                  )
                | Glue g
                | Drawing g ->(
                    let w=g.drawing_min_width+.comp*.(g.drawing_max_width-.g.drawing_min_width) in
                      page.pageContents<- (List.map (translate x y) (g.drawing_contents w)) @ page.pageContents;
                      w
                  )

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
            )
      ) p;

      let pnum=glyph_of_string env.substitutions env.positioning env.font env.size (string_of_int (i+1)) in
      let (_,w,_)=boxes_interval (Array.of_list pnum) in
      let x=(fst page.pageFormat -. w)/.2. in
        List.iter (function
                       (GlyphBox a)->
                         page.pageContents<- (Drivers.Glyph { a with glyph_x=x;glyph_y=20. }) :: page.pageContents
                     | _ -> ()
                  ) pnum;
        { page with pageContents=List.rev page.pageContents }
  in
  let a=Array.mapi draw_page opt_pages in
    (a, positions)
