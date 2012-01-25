open Drivers
open Binary

open Constants
open Bezier
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


type glyph = { contents:UTF8.t; glyph:Fonts.glyph; size: float; width:float; x0:float; x1:float; y0:float; y1:float }

type drawing=
    Curve of (float*float*curve)
  | Glyph of (float*float*glyph)


type glueBox = { glue_min_width:float; glue_max_width:float; glue_badness:float->float }
type drawingBox = { drawing_min_width:float; drawing_max_width:float;
                    drawing_y0:float->float; drawing_y1:float->float;
                    drawing_badness:float->float;
                    drawing:float->drawing list }

type box=
    GlyphBox of glyph
  | Glue of glueBox
  | Drawing of drawingBox
  | Mark of int
let is_glyph=function
    GlyphBox _->true
  | _->false


let glyphCache_=ref StrMap.empty

let glyphCache gl=
  let font=try StrMap.find (Fonts.fontName !current_font) !glyphCache_ with
        Not_found->(let fontCache=ref IntMap.empty in
                      glyphCache_:=StrMap.add (Fonts.fontName !current_font) fontCache !glyphCache_;
                      fontCache)
  in
    try IntMap.find gl !font with
        Not_found->
          (let glyph=Fonts.loadGlyph !current_font gl in
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
           let loaded=GlyphBox { contents=UTF8.init 1 (fun _->UChar.of_char ' ');
                                 glyph=glyph; size = !current_size;
                                 width=Fonts.glyphWidth glyph;
                                 x0=x0; x1=x1;
                                 y0=y0; y1=y1 } in
             
             font:=IntMap.add gl loaded !font;
             loaded)




let glyph_of_string fsize str =
  let rec make_codes idx codes=
    try
      let c=Fonts.glyph_of_char !current_font (UTF8.look str idx) in
        make_codes (UTF8.next str idx) (c::codes)
    with
        _->List.rev codes
  in
    (* List.iter (Printf.printf "%d ") (make_codes (UTF8.first str) []);Printf.printf "\n"; *)
  let codes=Fonts.transform !current_font (make_codes (UTF8.first str) []) in

    (* List.iter (Printf.printf "%d ") codes;Printf.printf "\n"; *)

    List.map (fun x->let GlyphBox y=glyphCache x in
                GlyphBox { y with (* contents=UTF8.init 1 (fun _->c); *) size = !current_size }) codes


let knuth_h_badness w1 w = 100.*.(abs_float (w-.w1)) ** 3.
