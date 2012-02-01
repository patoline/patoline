open Drivers
open Binary

open Constants
open Bezier
open CamomileLibrary
open FontsTypes

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

type glyph = { contents:UTF8.t; glyph:Fonts.glyph; width:float;
               x0:float; x1:float; y0:float; y1:float }

type drawing=
    Curve of (float*float*curve)
  | Glyph of (float*float*float*glyph)


type glueBox = { glue_min_width:float; glue_max_width:float; glue_badness:float->float }
type drawingBox = { drawing_min_width:float; drawing_max_width:float;
                    drawing_y0:float->float; drawing_y1:float->float;
                    drawing_badness:float->float;
                    drawing:float->drawing list }

type hyphenBox= { hyphen_normal:box array; hyphenated:(box array* box array) array }

and box=
    GlyphBox of (float*glyph)
  | Kerning of box kerningBox
  | Glue of glueBox
  | Drawing of drawingBox
  | Hyphen of hyphenBox
  | Mark of int

let is_glyph=function
    GlyphBox _->true
  | _->false


let glyphCache_=ref StrMap.empty

let glyphCache cur_font gl cont=
  let font=try StrMap.find (Fonts.fontName cur_font) !glyphCache_ with
        Not_found->(let fontCache=ref IntMap.empty in
                      glyphCache_:=StrMap.add (Fonts.fontName cur_font) fontCache !glyphCache_;
                      fontCache)
  in
    try IntMap.find gl !font with
        Not_found->
          (let glyph=Fonts.loadGlyph cur_font gl in
           let (y0,y1)=List.fold_left (fun (a,b) (_,y)->
                                         let (c,d)=Bezier.bernstein_extr y in
                                           (min a c, max b d)
                                      ) (1./.0., -1./.0.) (Fonts.outlines glyph)
           in
           let (x0,x1)=List.fold_left (fun (a,b) (y,_)->
                                         let (c,d)=Bezier.bernstein_extr y in
                                           (min a c, max b d)
                                      ) (1./.0., -1./.0.) (Fonts.outlines glyph)
           in
           let loaded={ contents=cont;
                        glyph=glyph; width=Fonts.glyphWidth glyph;
                        x0=x0; x1=x1;
                        y0=y0; y1=y1 } in
             
             font:=IntMap.add gl loaded !font;
             loaded)




let glyph_of_string font fsize str =
  let rec make_codes idx codes=
    try
      let c=Fonts.glyph_of_char font (UTF8.look str idx) in
        make_codes (UTF8.next str idx) (GlyphID (UTF8.init 1 (fun _->UTF8.look str idx),c)::codes)
    with
        _->List.rev codes
  in
  let codes=Fonts.substitutions font (make_codes (UTF8.first str) []) in
  let kerns=Fonts.kerning font codes in
    

  let rec kern=function
      GlyphID (c,h)::s ->let y=glyphCache font h c in GlyphBox (fsize, y)::kern s
    | KernID h::s->
        (match h.kern_contents with
             KernID h'->kern (KernID { advance_height=h.advance_height;
                                       advance_width=h.advance_width;
                                       kern_x0=h.kern_x0 +. h'.kern_x0;
                                       kern_y0=h.kern_y0 +. h'.kern_y0;
                                       kern_contents=h'.kern_contents }::s)
           | GlyphID (c,h')->(let y=glyphCache font h' c in 
                            Kerning { advance_height=h.advance_height*.(fsize)/.1000.;
                                      advance_width=h.advance_width*.(fsize)/.1000.;
                                      kern_x0=h.kern_x0*.(fsize)/.1000.;
                                      kern_y0=h.kern_y0*.(fsize)/.1000.;
                                      kern_contents=GlyphBox (fsize, y) }::(kern s))
        )
    | []->[]
  in
    kern kerns





let knuth_h_badness w1 w = 100.*.(abs_float (w-.w1)) ** 3.
