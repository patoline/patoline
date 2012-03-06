open Drivers
open Binary
open Fonts.FTypes
open Constants
open Bezier
open CamomileLibrary


type parameters={ lead:float;
                  measure:float;
                  lines_by_page:int;
                  left_margin:float;
                  local_optimization:int;
                  min_height_before:int;
                  min_height_after:int;
                  min_page_diff:int;
                  allow_widows:bool;
                  allow_orphans:bool
                }

let default_params={ lead=0.;
                     measure=0.;
                     lines_by_page=0;
                     left_margin=0.;
                     local_optimization=0;
                     min_height_before=1;
                     min_height_after=1;
                     min_page_diff=0;
                     allow_widows=true;
                     allow_orphans=true }

let print_parameters p=
  Printf.printf "{ lead=%f; measure=%f; lines_by_page=%d; left_margin=%f }\n"
    p.lead p.measure p.lines_by_page p.left_margin

type line= { paragraph:int; lastFigure:int; lineEnd:int; lineStart:int; hyphenStart:int; hyphenEnd:int;
             isFigure:bool; mutable height:int; paragraph_height:int; mutable page_height:int; mutable page:int;
             min_width:float; nom_width:float; max_width:float
           }

module Line=struct
  type t=line
  let compare line0 line1=
    compare line0 line1
end
module LineMap=New_map.Make(Line)



type 'a kerningBox='a Fonts.FTypes.kerningBox

type drawingBox = { drawing_min_width:float; drawing_nominal_width:float;
                    drawing_max_width:float; drawing_y0:float; drawing_y1:float;
                    drawing_badness : float -> float;
                    drawing_contents:float -> Drivers.contents list }

and glueBox = { glue_min_width:float; glue_max_width:float; glue_nominal_width: float; glue_badness:float->float;
                glue_contents : float -> Drivers.contents list }

and hyphenBox= { hyphen_normal:box array; hyphenated:(box array* box array) array }

and box=
    GlyphBox of glyph
  | Kerning of box kerningBox
  | Glue of drawingBox
  | Drawing of drawingBox
  | Hyphen of hyphenBox
  | Empty

let drawing yOff cont=
  let (a,b,c,d)=Drivers.bounding_box cont in
    {
      drawing_min_width=c-.a;
      drawing_nominal_width=c-.a;
      drawing_max_width=c-.a;
      drawing_y0=yOff+.b;
      drawing_y1=yOff+.d;
      drawing_badness=(fun _->0.);
      drawing_contents=(fun _->cont)
    }

type error_log=
    Overfull_line of line
  | Widow of line
  | Orphan of line

type citation= { citation_paragraph:int; citation_box:int }

let print_line l=
  Printf.printf "{ paragraph=%d; lineStart=%d; lineEnd=%d; hyphenStart=%d; hyphenEnd=%d; lastFigure=%d; height=%d; page=%d }\n"
    l.paragraph l.lineStart l.lineEnd l.hyphenStart l.hyphenEnd l.lastFigure l.height l.page
let rec print_box=function
    Glue _->Printf.printf " "
  | GlyphBox x->Printf.printf "%s" (Fonts.glyphContents x.glyph)
  | Kerning x->print_box x.kern_contents
  | Hyphen x->Array.iter print_box x.hyphen_normal
  | _->Printf.printf "[]"

let rec print_box_type=function
    Glue _->Printf.printf "Glue "
  | GlyphBox _->Printf.printf "GlyphBox "
  | Kerning x->Printf.printf "Kerning "
  | Hyphen x->Printf.printf "Hyphen "
  | _->Printf.printf "[]"


let print_text_line lines node=
  print_line node;
  for i=node.lineStart to node.lineEnd-1 do
    print_box (lines.(node.paragraph).(i))
  done;
  print_newline()


let fold_left_line paragraphs f x0 line=
   let rec fold boxes i maxi result=
    if i>=maxi then result else
      match boxes.(i) with
          Hyphen h -> fold boxes (i+1) maxi
            (fold h.hyphen_normal 0 (Array.length h.hyphen_normal) result)
        | b -> fold boxes (i+1) maxi (f result b)
  in
  let x1=
    if line.hyphenStart>=0 then
      (match paragraphs.(line.paragraph).(line.lineStart) with
           Hyphen h->fold paragraphs.(line.paragraph) (line.lineStart+1) line.lineEnd (
             let _,boxes=h.hyphenated.(line.hyphenStart) in
               fold boxes 0 (Array.length boxes) x0
           )
         | _ -> fold paragraphs.(line.paragraph) line.lineStart line.lineEnd x0)
    else
      (fold paragraphs.(line.paragraph) line.lineStart line.lineEnd x0)
  in
    if line.hyphenEnd>=0 then
      (match paragraphs.(line.paragraph).(line.lineEnd) with
         Hyphen h->(
             let boxes,_=h.hyphenated.(line.hyphenEnd) in
               fold boxes 0 (Array.length boxes) x1
           )
         | _ -> x1)
    else
      x1

let first_line paragraphs line=
  let rec find boxes i=
    match boxes.(i) with
        Hyphen x->find (x.hyphen_normal) 0
      | _-> boxes.(i)
  in
    if line.hyphenStart<0 then (
      find paragraphs.(line.paragraph) line.lineStart
    ) else (
      match paragraphs.(line.paragraph).(line.lineStart) with
          Hyphen x-> let b,_=x.hyphenated.(line.hyphenStart) in find b 0
        | b -> b
    )
let last_line paragraphs line=
  let rec find boxes i=
    match boxes.(i) with
        Hyphen x->find (x.hyphen_normal) (Array.length x.hyphen_normal-1)
      | _-> boxes.(i)
  in
    if line.hyphenEnd<0 then (
      find paragraphs.(line.paragraph) (line.lineEnd-1)
    ) else (
      match paragraphs.(line.paragraph).(line.lineEnd) with
          Hyphen x-> let _,b = x.hyphenated.(line.hyphenEnd) in find b (Array.length b-1)
        | b -> b
    )




let is_glyph=function
    GlyphBox _->true
  | _->false
let is_glue=function
    Glue _->true
  | _->false
let is_hyphen =function Hyphen _->true | _->false




let rec box_width comp=function
    GlyphBox x->Fonts.glyphWidth x.glyph*.x.glyph_size/.1000.
  | Glue x
  | Drawing x->x.drawing_min_width+.comp*.(x.drawing_max_width -. x.drawing_min_width)
  | Kerning x->(box_width comp x.kern_contents) +. x.advance_width
  | Hyphen x->Array.fold_left (fun s x->s+.box_width comp x) 0. x.hyphen_normal
  | Empty->0.


let rec box_interval=function
    GlyphBox x->let y=Fonts.glyphWidth x.glyph*.x.glyph_size/.1000. in (y,y,y)
  | Glue x
  | Drawing x->x.drawing_min_width, x.drawing_nominal_width, x.drawing_max_width
  | Kerning x->let (a,b,c)=box_interval x.kern_contents in (a +. x.advance_width, b +. x.advance_width, c+. x.advance_width)
  | Hyphen x->boxes_interval x.hyphen_normal
  | _->(0.,0.,0.)

and boxes_interval boxes=
  let a=ref 0. in let b=ref 0. in let c=ref 0. in
    for i=0 to Array.length boxes-1 do
      let (u,v,w)=box_interval (boxes.(i)) in
        a:= !a+.u;b:= !b+.v;c:= !c+.w
    done;
    (!a,!b,!c)

let rec lower_y x w=match x with
    GlyphBox y->Fonts.glyph_y0 y.glyph*.y.glyph_size/.1000.
  | Glue y
  | Drawing y->y.drawing_y0
  | Kerning y->(lower_y y.kern_contents w) +. y.kern_y0
  | _->0.

let rec upper_y x w=match x with
    GlyphBox y->Fonts.glyph_y1 y.glyph*.y.glyph_size/.1000.
  | Glue y
  | Drawing y->y.drawing_y1
  | Kerning y->(upper_y y.kern_contents w) +. y.kern_y0
  | _-> 0.

let line_height paragraphs node=
  let rec line_height boxes k maxk min_height max_height=
    if k>=maxk then (min_height,max_height) else (
      match boxes.(k) with
          Hyphen x->(
            let a,b=line_height x.hyphen_normal 0 (Array.length x.hyphen_normal) min_height max_height in
              line_height boxes (k+1) maxk a b
          )
        | _->(line_height boxes (k+1) maxk
                (min min_height (lower_y boxes.(k) 0.))
                (max max_height (upper_y boxes.(k) 0.)))
    )
  in
  let a0,b0=
    if node.hyphenStart>=0 then (
      match paragraphs.(node.paragraph).(node.lineStart) with
          Hyphen x->
            (let hyp=snd x.hyphenated.(node.hyphenStart) in
             let u,v=line_height hyp 0 (Array.length hyp) 0. 0. in
               line_height paragraphs.(node.paragraph) (node.lineStart+1) node.lineEnd u v)
        | _->line_height paragraphs.(node.paragraph) (node.lineStart+1) node.lineEnd 0. 0.
    ) else (line_height paragraphs.(node.paragraph) (node.lineStart) node.lineEnd 0. 0.)
  in
    if node.hyphenEnd>=0 then (
      match paragraphs.(node.paragraph).(node.lineEnd) with
          Hyphen x->let hyp=fst x.hyphenated.(node.hyphenEnd) in
            line_height hyp 0 (Array.length hyp) a0 b0
        | _->a0,b0
    ) else a0,b0

let comp paragraphs m p i hi j hj=
  let minLine=ref 0. in
  let maxLine=ref 0. in
  let nomLine=ref 0. in
    if hi>=0 then (
      match paragraphs.(p).(i) with
          Hyphen x->let a,b,c=boxes_interval (snd x.hyphenated.(hi)) in
            (minLine:= !minLine+.a;
             nomLine:= !nomLine+.c;
             maxLine:= !maxLine+.b)
        | _->());
    if hj>=0 then (
      match paragraphs.(p).(j) with
          Hyphen x->let a,b,c=boxes_interval (fst x.hyphenated.(hj)) in
            (minLine:= !minLine+.a;
             nomLine:= !nomLine+.b;
             maxLine:= !maxLine+.c)
        | _->());
    for k=(if hi<0 then i else i+1) to j-1 do
      let a,b,c=box_interval paragraphs.(p).(k) in
        minLine := !minLine+.a;
        nomLine := !nomLine+.b;
        maxLine := !maxLine+.c
    done;
    if !maxLine = !minLine then 0. else (
      let com= max 0. ((m-. !minLine)/.(!maxLine-. !minLine)) in
        if com>=1. then ((!nomLine-. !minLine)/.(!maxLine-. !minLine)) else com
    )
let compression paragraphs (parameters,line)=comp paragraphs parameters.measure
  line.paragraph line.lineStart line.hyphenStart line.lineEnd line.hyphenEnd




let glyphCache_=ref StrMap.empty

let glyphCache cur_font gl=
  let font=try StrMap.find (Fonts.fontName cur_font) !glyphCache_ with
        Not_found->(let fontCache=ref IntMap.empty in
                      glyphCache_:=StrMap.add (Fonts.fontName cur_font) fontCache !glyphCache_;
                      fontCache)
  in
    try IntMap.find gl.glyph_index !font with
        Not_found->
          (let glyph=Fonts.loadGlyph cur_font gl in
           let loaded={ glyph=glyph;
                        glyph_x=0.;glyph_y=0.;
                        glyph_color=black;
                        glyph_size=infinity } in
             font:=IntMap.add gl.glyph_index loaded !font;
             loaded)


let glyph_of_string substitution_ positioning_ font fsize str =
  let rec make_codes idx codes=
    try
      let c=Fonts.glyph_of_uchar font (UTF8.look str idx) in
        make_codes (UTF8.next str idx) ({glyph_utf8=UTF8.init 1 (fun _->UTF8.look str idx); glyph_index=c}::codes)
    with
        _->List.rev codes
  in
  let codes=substitution_ (make_codes (UTF8.first str) []) in
  let kerns=positioning_ (List.map (fun x->GlyphID x) codes) in

  let rec kern=function
      GlyphID h::s ->let y=glyphCache font h in GlyphBox { y with glyph_size=fsize }::kern s
    | KernID h::s->
        (match h.kern_contents with
             KernID h'->kern (KernID { advance_height=h.advance_height;
                                       advance_width=h.advance_width;
                                       kern_x0=h.kern_x0 +. h'.kern_x0;
                                       kern_y0=h.kern_y0 +. h'.kern_y0;
                                       kern_contents=h'.kern_contents }::s)
           | GlyphID c->(let y=glyphCache font c in
                            Kerning { advance_height=h.advance_height*.(fsize)/.1000.;
                                      advance_width=h.advance_width*.(fsize)/.1000.;
                                      kern_x0=h.kern_x0*.(fsize)/.1000.;
                                      kern_y0=h.kern_y0*.(fsize)/.1000.;
                                      kern_contents=GlyphBox { y with glyph_size=fsize } }::(kern s))
        )
    | []->[]
  in
    kern kerns


let hyphenate hyph subs kern font fsize str=
  let hyphenated=hyph str in
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

let rec resize l=function
    GlyphBox b -> GlyphBox { b with glyph_size= l*.b.glyph_size }
  | Hyphen x->Hyphen { hyphen_normal=Array.map (resize l) x.hyphen_normal;
                       hyphenated=Array.map (fun (a,b)->Array.map (resize l) a, Array.map (resize l) b) x.hyphenated }
  | Glue x -> Glue { x with
                       drawing_min_width= x.drawing_min_width*.l;
                       drawing_max_width= x.drawing_max_width*.l;
                       drawing_nominal_width= x.drawing_nominal_width*.l;
                       drawing_badness = knuth_h_badness (2.*.(x.drawing_max_width+.x.drawing_min_width)/.3.) }
  | Kerning x -> Kerning { advance_width = l*.x.advance_width;
                           advance_height = l*.x.advance_height;
                           kern_x0 = l*.x.kern_x0;
                           kern_y0 = l*.x.kern_y0;
                           kern_contents=resize l x.kern_contents }
  | x->x
