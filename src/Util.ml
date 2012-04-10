open OutputCommon
open Binary
open Fonts.FTypes
open Constants
open Bezier
open CamomileLibrary


type line= {
  paragraph:int;                        (** L'indice du paragraphe dans le tableau *)
  lastFigure:int;                       (** La dernière figure placée, initialement -1 *)
  lineStart:int;                        (** Le numéro de la boite de début dans le paragraphe *)
  lineEnd:int;                          (** Le numéro de la boite suivant la dernière boite de la ligne, ou, si la ligne est césurée, le numéro de la boite contenant la césure *)
  hyphenStart:int;
  hyphenEnd:int;
  isFigure:bool;
  mutable height:float;
  paragraph_height:int;
  mutable page_line:int;
  mutable page:int;
  min_width:float;
  nom_width:float;
  max_width:float
}

let uselessLine=
  { paragraph=0; lineStart= -1; lineEnd= -1; hyphenStart= -1; hyphenEnd= -1; isFigure=false;
    lastFigure=(-1); height= 0.;paragraph_height= -1; page_line=0; page=0;
    min_width=0.;nom_width=0.;max_width=0. }


type parameters={ measure:float;
                  page_height:float;
                  left_margin:float;
                  local_optimization:int;
                  next_acceptable_height:line->parameters->line->parameters->float;
                  min_height_before:float;
                  min_height_after:float;
                  min_page_before:int;
                  min_page_after:int
                }

let default_params={ measure=0.;
                     page_height=0.;
                     left_margin=0.;
                     local_optimization=0;
                     next_acceptable_height=(fun _ _ h _ ->h.height);
                     min_height_before=0.;
                     min_height_after=0.;
                     min_page_before=0;
                     min_page_after=0
                   }

type 'a kerningBox='a Fonts.FTypes.kerningBox

type drawingBox = { drawing_min_width:float; drawing_nominal_width:float;
                    drawing_max_width:float; drawing_y0:float; drawing_y1:float;
                    drawing_badness : float -> float;
                    drawing_contents:float -> OutputCommon.contents list }


and 'a hyphenBox= { hyphen_normal:'a box array; hyphenated:('a box array* 'a box array) array }

and 'a box=
    GlyphBox of glyph
  | Kerning of 'a box kerningBox
  | Glue of drawingBox
  | Drawing of drawingBox
  | Hyphen of 'a hyphenBox
  | User of 'a
  | Empty


let drawing ?offset:(offset=0.) cont=
  let (a,b,c,d)=OutputCommon.bounding_box cont in
    {
      drawing_min_width=c-.a;
      drawing_nominal_width=c-.a;
      drawing_max_width=c-.a;
      drawing_y0=offset;
      drawing_y1=offset+.d-.b;
      drawing_badness=(fun _->0.);
      drawing_contents=(fun _->List.map (translate (-.a) (offset-.b)) cont)
    }

let drawing_blit a x0 y0 b=
  let w0=max a.drawing_min_width (x0+.b.drawing_min_width) in
  let w1=max a.drawing_max_width (x0+.b.drawing_max_width) in
    { drawing_min_width = w0;
      drawing_nominal_width = max a.drawing_nominal_width (x0+.b.drawing_nominal_width);
      drawing_max_width = w0;
      drawing_y0=min a.drawing_y0 (y0+.b.drawing_y0);
      drawing_y1=max a.drawing_y1 (y0+.b.drawing_y1);
      drawing_badness=(fun w->
                         let fact=w/.(w1-.w0) in
                           a.drawing_badness ((a.drawing_max_width-.a.drawing_min_width)*.fact)
                           +.b.drawing_badness ((b.drawing_max_width-.b.drawing_min_width)*.fact));
      drawing_contents=(fun w->
                          let fact=w/.(w1-.w0) in
                          let ca=a.drawing_contents ((a.drawing_max_width-.a.drawing_min_width)*.fact) in
                          let cb=b.drawing_contents ((b.drawing_max_width-.b.drawing_min_width)*.fact) in
                            (List.map (translate x0 y0) cb)@ca)
    }

let print_linef out l=
  Printf.fprintf out "{ paragraph=%d; lineStart=%d; lineEnd=%d; hyphenStart=%d; hyphenEnd=%d; lastFigure=%d; height=%f; page=%d }\n"
    l.paragraph l.lineStart l.lineEnd l.hyphenStart l.hyphenEnd l.lastFigure l.height l.page
let print_line l=print_linef stdout l

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
  if line.paragraph>=Array.length paragraphs || line.paragraph<0 then x0 else (
    let rec fold boxes i maxi result=
      if i>=maxi || i>=Array.length boxes then result else
        match boxes.(i) with
            Hyphen h -> fold boxes (i+1) maxi
              (fold h.hyphen_normal 0 (Array.length h.hyphen_normal) result)
          | b -> fold boxes (i+1) maxi (f result b)
    in
    let x1=
      if line.hyphenStart>=0 then
        (match paragraphs.(line.paragraph).(line.lineStart) with
             Hyphen h->fold paragraphs.(line.paragraph) (max 0 (line.lineStart+1)) line.lineEnd (
               let _,boxes=h.hyphenated.(line.hyphenStart) in
                 fold boxes 0 (Array.length boxes) x0
             )
           | _ -> fold paragraphs.(line.paragraph) (max 0 line.lineStart) line.lineEnd x0)
      else
        (fold paragraphs.(line.paragraph) (max 0 line.lineStart) line.lineEnd x0)
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
  )

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
  | _->0.


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

let draw_boxes l=
  let rec draw_box x y dr=function
      Kerning kbox ->(
        let dr',w=draw_box (x+.kbox.kern_x0) (y+.kbox.kern_y0) dr kbox.kern_contents in
          dr', w+.kbox.advance_width
      )
    | Hyphen h->(
        Array.fold_left (fun (dr',x') box->
                           let dr'',w=draw_box (x+.x') y dr' box in
                             dr'',x'+.w
                        ) (dr,0.) h.hyphen_normal
      )
    | GlyphBox a->(
        ((OutputCommon.Glyph { a with glyph_x=a.glyph_x+.x;glyph_y=a.glyph_y+.y }) :: dr),
        a.glyph_size*.Fonts.glyphWidth a.glyph/.1000.
      )
    | Glue g
    | Drawing g ->(
        let w=g.drawing_nominal_width in
          (List.map (translate (x) (y)) (g.drawing_contents w)) @ dr, w
      )
    | b->dr,(box_width 0. b)
  in
  let rec make_line x y res=function
      []->res
    | h::s->(
        let dr,w=draw_box x y res h in
          make_line (x+.w) y dr s
      )
  in
    make_line 0. 0. [] l


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
                        glyph_size=0. } in
             font:=IntMap.add gl.glyph_index loaded !font;
             loaded)


let glyph_of_string substitution_ positioning_ font fsize fcolor str =
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
      GlyphID h::s ->let y=glyphCache font h in GlyphBox { y with glyph_color=fcolor; glyph_size=fsize }::kern s
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
                                      kern_contents=GlyphBox { y with glyph_color=fcolor; glyph_size=fsize } }::(kern s))
        )
    | []->[]
  in
    kern kerns

let hyphenate hyph subs kern font fsize fcolor str=
  let hyphenated=hyph str in
    if Array.length hyphenated=0 then
      glyph_of_string subs kern font fsize fcolor str
    else
      [Hyphen { hyphen_normal=Array.of_list (glyph_of_string subs kern font fsize fcolor str);
                hyphenated=Array.map (fun (a,b)->(Array.of_list (glyph_of_string subs kern font fsize fcolor a),
                                                  Array.of_list (glyph_of_string subs kern font fsize fcolor b))) hyphenated }]


let knuth_h_badness w1 w = 100.*.(abs_float (w-.w1)) ** 3.
let glue a b c=
  Glue { drawing_min_width= a;
         drawing_max_width= c;
         drawing_y0=0.; drawing_y1=0.;
         drawing_nominal_width= b;
         drawing_contents=(fun _->[]);
         drawing_badness=knuth_h_badness b }

let rec resize l=function
    GlyphBox b -> GlyphBox { b with glyph_size= l*.b.glyph_size }
  | Hyphen x->Hyphen { hyphen_normal=Array.map (resize l) x.hyphen_normal;
                       hyphenated=Array.map (fun (a,b)->Array.map (resize l) a, Array.map (resize l) b) x.hyphenated }
  | Drawing x -> Drawing {
                       drawing_min_width= x.drawing_min_width*.l;
                       drawing_max_width= x.drawing_max_width*.l;
                       drawing_y0=x.drawing_y0*.l;
                       drawing_y1=x.drawing_y1*.l;
                       drawing_nominal_width= x.drawing_nominal_width*.l;
                       drawing_badness = knuth_h_badness (2.*.(x.drawing_max_width+.x.drawing_min_width)/.3.);
                       drawing_contents=(fun w->List.map (OutputCommon.resize l) (x.drawing_contents w))
                   }
  | Glue x -> Glue {
                       drawing_min_width= x.drawing_min_width*.l;
                       drawing_max_width= x.drawing_max_width*.l;
                       drawing_y0=x.drawing_y0*.l;
                       drawing_y1=x.drawing_y1*.l;
                       drawing_nominal_width= x.drawing_nominal_width*.l;
                       drawing_badness = knuth_h_badness (2.*.(x.drawing_max_width+.x.drawing_min_width)/.3.);
                       drawing_contents=(fun w->List.map (OutputCommon.resize l) (x.drawing_contents w))
                   }
  | Kerning x -> Kerning { advance_width = l*.x.advance_width;
                           advance_height = l*.x.advance_height;
                           kern_x0 = l*.x.kern_x0;
                           kern_y0 = l*.x.kern_y0;
                           kern_contents=resize l x.kern_contents }
  | x->x
