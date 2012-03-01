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
    let page= { pageFormat=0.,0. ; pageContents=[] } in
      List.iter (
        fun (param,line)->
          let (h,w)=page.pageFormat in
          let (h',w')=param.format in
            page.pageFormat<- (max h h', max w w');
            let y=270.0-.param.lead*.float_of_int line.height in
              if line.paragraph<> !par then (par:=line.paragraph; positions.(!par)<-(i,0., y+. param.lead/.2. +. snd (line_height paragraphs line)));

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
                page.pageContents<- (Drivers.Glyph { glyph_x=x;glyph_y=y; glyph_color=black;
                                                     glyph_size=size;
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

      let pnum=glyph_of_string env.substitutions env.positioning env.font env.size (string_of_int (i+1)) in
      let (_,w,_)=boxes_interval (Array.of_list pnum) in
      let x=(fst page.pageFormat -. w)/.2. in
        List.iter (function
                       (GlyphBox (s,a))->
                         page.pageContents<- (Drivers.Glyph { glyph_x=x;glyph_y=20.; glyph_color=black;
                                                              glyph_size=s;
                                                              glyph=a.Util.glyph }) :: page.pageContents
                     | _ -> ()
                  ) pnum;
        page
  in
  let a=Array.mapi draw_page opt_pages in
    (a, positions)
