open Drivers
open Binary
open Boxes
open Constants

open CamomileLibrary

let array_of_rev_list l0=
  match l0 with
      []->[||]
    | h0::_->
        let arr=Array.create (List.length l0) h0 in
        let rec do_it l i=match l with
            []->arr
          | h::s->(arr.(i)<-h; do_it s (i-1))
        in
          do_it l0 (Array.length arr-1)

let current_font=ref (Fonts.loadFont "AGaramondPro-Regular.otf")
let current_size=ref 4.

let glyphCache_=ref StrMap.empty

let glyphCache gl=
  let font=try StrMap.find (Fonts.fontName !current_font) !glyphCache_ with
        Not_found->(let fontCache=ref IntMap.empty in
                      glyphCache_:=StrMap.add (Fonts.fontName !current_font) fontCache !glyphCache_;
                      fontCache)
  in
  let code=UChar.code gl in
    try IntMap.find code !font with
        Not_found->
          (let loaded=Fonts.loadGlyph !current_font (Fonts.glyph_of_char !current_font gl) in
             font:=IntMap.add code loaded !font;
             loaded)

let glyph_of_string fsize str =

  let rec make_glyphs idx glyphs=
    try
      let c=UTF8.look str idx in
      let gl=glyphCache c in
      let (y0,y1)=List.fold_left (fun (a,b) (_,y)->
                                    let (c,d)=Bezier.bernstein_extr y in
                                      (min a c, max b d)
                                 ) (1./.0., -1./.0.) (Fonts.outlines gl)
      in
      let (x0,x1)=List.fold_left (fun (a,b) (y,_)->
                                    let (c,d)=Bezier.bernstein_extr y in
                                      (min a c, max b d)
                                 ) (1./.0., -1./.0.) (Fonts.outlines gl)
      in
        make_glyphs (UTF8.next str idx)
          (GlyphBox { contents=UTF8.init 1 (fun _->c); glyph=gl; size = fsize; width=Fonts.glyphWidth gl;
                      x0=x0; x1=x1;
                      y0=y0; y1=y1 } :: glyphs)
    with
        _->glyphs
  in
    make_glyphs (UTF8.first str) []


let knuth_h_badness w1 w = 100.*.(abs_float (w-.w1)) ** 3.
