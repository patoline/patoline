open Drivers
open Binary
open FontsTypes
open Constants
open Bezier
open CamomileLibrary


type parameters={ format:float*float;
                  lead:float;
                  measure:float;
                  lines_by_page:int;
                  left_margin:float }

type line= { paragraph:int; lineStart:int; lineEnd:int; hyphenStart:int; hyphenEnd:int;
             lastFigure:int; height:int; paragraph_height:int; page:int }

module Line=struct
  type t=line
  let compare line0 line1=
    compare line0 line1
end
module LineMap=Map.Make(Line)



type 'a kerningBox='a FontsTypes.kerningBox

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
  | Empty
let is_glyph=function
    GlyphBox _->true
  | _->false
let is_glue=function
    Glue _->true
  | _->false
let rec box_width comp=function
    GlyphBox (size,x)->x.width*.size/.1000.
  | Glue x->(x.glue_min_width+.(x.glue_max_width-.x.glue_min_width)*.comp)
  | Drawing x->(x.drawing_min_width+.(x.drawing_max_width-.x.drawing_min_width)*.comp)
  | Kerning x->(box_width comp x.kern_contents) +. x.advance_width
  | Hyphen x->Array.fold_left (fun s x->s+.box_width comp x) 0. x.hyphen_normal
  | Empty->0.
  | _->0.

let rec box_interval=function
    GlyphBox (size,x)->let y=x.width*.size/.1000. in (y,y)
  | Glue x->(x.glue_min_width, x.glue_max_width)
  | Drawing x->(x.drawing_min_width, x.drawing_max_width)
  | Kerning x->let (a,b)=box_interval x.kern_contents in (a +. x.advance_width, b +. x.advance_width)
  | Hyphen x->boxes_interval x.hyphen_normal
  | _->(0.,0.)

and boxes_interval boxes=
  let a=ref 0. in let b=ref 0. in
    for i=0 to Array.length boxes-1 do
      let (u,v)=box_interval (boxes.(i)) in
        a:= !a+.u;b:= !b+.v
    done;
    (!a,!b)

let rec lower_y x w=match x with
    GlyphBox (size,y)->y.y0*.size/.1000.
  | Drawing y->y.drawing_y0 w
  | Glue _->0.
  | Kerning y->(lower_y y.kern_contents w) +. y.kern_y0
  | _->0.

let rec upper_y x w=match x with
    GlyphBox (size,y)->y.y1*.size/.1000.
  | Drawing y->y.drawing_y1 w
  | Glue _->0.
  | Kerning y->(upper_y y.kern_contents w) +. y.kern_y0
  | _-> 0.


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


let glyph_of_string substitution_ positioning_ font fsize str =
  let rec make_codes idx codes=
    try
      let c=Fonts.glyph_of_char font (UTF8.look str idx) in
        make_codes (UTF8.next str idx) (GlyphID (UTF8.init 1 (fun _->UTF8.look str idx),c)::codes)
    with
        _->List.rev codes
  in
  let codes=substitution_ (make_codes (UTF8.first str) []) in
  let kerns=positioning_ codes in

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


let hyphenate tree subs kern font fsize str=
  let hyphenated=Hyphenate.hyphenate str tree in
  let pos=Array.make (List.length hyphenated-1) ([||],[||]) in
  let rec hyph l i cur=match l with
      []->[Hyphen { hyphen_normal=Array.of_list (glyph_of_string subs kern font fsize str); hyphenated=pos }]
    | h::s->(
        pos.(i)<-(Array.of_list (glyph_of_string subs kern font fsize (cur^"-")),
                  Array.of_list (glyph_of_string subs kern font fsize (List.fold_left (^) "" l)));
        hyph s (i+1) (cur^h)
      )
  in
    match hyphenated with
        []->glyph_of_string subs kern font fsize str
      | h::s->hyph s 0 h

let knuth_h_badness w1 w = 100.*.(abs_float (w-.w1)) ** 3.
