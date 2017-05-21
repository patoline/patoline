(*
  Copyright Florian Hatat, Tom Hirschowitz, Pierre Hyvernat,
  Pierre-Etienne Meunier, Christophe Raffalli, Guillaume Theyssier 2012.

  This file is part of Patoline.

  Patoline is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Patoline is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Patoline.  If not, see <http://www.gnu.org/licenses/>.
*)
open RawContent
open Util
open UsualMake
open FTypes
open Bezier

type box =
  | GlyphBox    of RawContent.glyph
  | Kerning     of box FTypes.kerningBox
  | Glue        of drawingBox
  | Drawing     of drawingBox
  | Hyphen      of hyphenBox
  | Marker      of marker
  | BeginFigure of int
  | FlushFigure of int
  | Parameters  of (parameters->parameters)
  | Layout      of (frame_zipper->frame_zipper)
  | Empty

and drawingBox =
  { drawing_min_width     : float
  ; drawing_nominal_width : float
  ; drawing_max_width     : float
  ; drawing_width_fixed   : bool
  ; drawing_adjust_before : bool
  ; drawing_y0            : float
  ; drawing_y1            : float
  ; drawing_badness       : float -> float
  ; drawing_break_badness : float
  ; drawing_states        : int list
  ; drawing_contents      : float -> RawContent.raw list }

and hyphenBox =
  { hyphen_normal : box array
  ; hyphenated    : (box array * box array) array }

and kind =
  | Extern of string
  | Intern of string
  | Button of button_kind * string

and marker =
  | Label     of string
  | FigureRef of int
  | Pageref   of string
  | Structure of int list
  | BeginLink of kind
  | EndLink
  | AlignmentMark

and line =
  (** The index of the paragraph in the array. *)
  { paragraph        : int
  (** The last placed figure (initially -1) *)
  ; lastFigure       : int
  (** The index of the first box of the line in the paragraph. *)
  ; lineStart        : int
  (** The index of the box next to the last box of the line, or the box with
      the hyphenation if the line is hyphenated. *)
  ; lineEnd          : int
  ; hyphenStart      : int
  ; hyphenEnd        : int
  ; isFigure         : bool
  ; layout           : frame_zipper
  ; height           : float
  ; paragraph_height : int
  ; page_line        : int
  ; min_width        : float
  ; nom_width        : float
  ; max_width        : float
  ; line_y0          : float
  ; line_y1          : float }

and parameters =
  { measure            : float
  ; left_margin        : float
  ; local_optimization : int
  ; min_height_before  : float
  ; min_height_after   : float
  ; min_page_before    : int
  ; min_page_after     : int
  ; not_last_line      : bool
  ; not_first_line     : bool
  ; min_lines_before   : int
  ; min_lines_after    : int
  ; absolute           : bool }

and placed_line =
  { line_params : parameters
  ; line        : line }

and frame_content =
  | Placed_line of placed_line
  | Raw         of RawContent.raw list

and frame =
  { frame_children : frame IntMap.t
  ; frame_tags     : string list
  ; frame_x0       : float
  ; frame_y0       : float
  ; frame_x1       : float
  ; frame_y1       : float
  ; frame_content  : frame_content list }

and frame_zipper = frame * (int * frame) list

module MarkerMap = Map.Make(
  struct
    type t = marker
    let compare m1 m2 = match m1, m2 with
      | BeginLink l1, BeginLink l2 ->
         begin
           match l1, l2 with
           | Button(_,n1), Button(_,n2) -> String.compare n1 n2
           | _ -> compare l1 l2
         end
      | _ -> compare m1 m2
  end)

(** Helpful test functions. *)
let is_glyph  : box -> bool = function GlyphBox _ -> true | _ -> false
let is_glue   : box -> bool = function Glue _     -> true | _ -> false
let is_hyphen : box -> bool = function Hyphen _   -> true | _ -> false

(** Build a box containing one glyph, with a cache. *)
let glyphCache : Fonts.font -> glyph_id -> Color.color -> float -> box =
  let module GMap = Map.Make(
    struct
      type t = string * int * float * Color.color
      let compare = compare
    end) in
  let cache = ref GMap.empty in
  let glyphCache cur_font gl glyph_color glyph_size =
    let name = Fonts.uniqueName cur_font in
    let key = (name, gl.glyph_index, glyph_size, glyph_color) in
    try GMap.find key !cache with Not_found ->
      let loaded = GlyphBox
        { glyph   = Fonts.loadGlyph cur_font gl
        ; glyph_x = 0.0 ; glyph_kx = 0.0
        ; glyph_y = 0.0 ; glyph_ky = 0.0
        ; glyph_order = 0 ; glyph_color ; glyph_size }
      in
      cache := GMap.add key loaded !cache; loaded
  in glyphCache

(** Build a glyph id from a font and a unicode character. *)
let glyph_of_uchar : Fonts.font -> UChar.t ->FTypes.glyph_id = fun font u ->
  { glyph_index = Fonts.glyph_of_uchar font u
  ; glyph_utf8  = UTF8.encode u }

(** Build a list of boxes from a string, using stuff from kerning. *)
let glyph_of_string subs kern font size color str =
  let rec glyphs i gs =
    if i >= String.length str then List.rev gs else
      let g = glyph_of_uchar font (UTF8.look str i) in
      glyphs (UTF8.next str i) (g :: gs)
  in
  let codes = subs (glyphs 0 []) in
  let codes = kern (List.map (fun x -> GlyphID x) codes) in
  let rec kern = function
    | GlyphID h :: s -> (glyphCache font h color size) :: (kern s)
    | KernID h  :: s ->
        begin
          match h.kern_contents with
          | KernID h' ->
              let k =
                { advance_height = h.advance_height
                ; advance_width  = h.advance_width
                ; kern_x0 = h.kern_x0 +. h'.kern_x0
                ; kern_y0 = h.kern_y0 +. h'.kern_y0
                ; kern_contents = h'.kern_contents }
              in kern (KernID k :: s)
          | GlyphID c ->
              let kern_contents = glyphCache font c color size in
              let m = size /. 1000.0 in
              let advance_height = h.advance_height *. m in
              let advance_width  = h.advance_width  *. m in
              let kern_x0 = h.kern_x0 *. m in
              let kern_y0 = h.kern_y0 *. m in
              let k =
                { advance_height ; advance_width ; kern_x0 ; kern_y0
                ; kern_contents }
              in (Kerning k) :: (kern s)
        end
    | [] -> []
  in kern codes

(** Build a list of boxes representing a word, while taking care of building
    an array of possible hyphenations for the word (if any). *)
let hyphenate hyph subs kern font size color str =
  let of_string = glyph_of_string subs kern font size color in
  let hyphenated = hyph str in
  if hyphenated = [||] then of_string str else
    let hyphen_normal = Array.of_list (of_string str) in
    let hyphenated =
      let f (a,b) =
        (Array.of_list (of_string a), Array.of_list (of_string b))
      in
      Array.map f hyphenated
    in
    [Hyphen { hyphen_normal ; hyphenated }]





(* Helper functions for layouts *)

let frame_up (t,cxt)=
  match cxt with
      []->(t,cxt)
    | (i,t')::s->({t' with frame_children=IntMap.add i t t'.frame_children},s)

let frame_down i (t,cxt)=
  (IntMap.find i t.frame_children, ((i,t)::cxt))

let frame_down_last (t,ctxt as f) =
  try
    let (i,_) = IntMap.max_binding t.frame_children in
    frame_down i f
  with Not_found -> f

let rec frame_top (t,cxt)=
  match cxt with
      []->(t,cxt)
    | (i,t')::s->frame_top ({t' with frame_children=IntMap.add i t t'.frame_children},s)

let empty_frame =
  { frame_children = IntMap.empty ; frame_tags = [] ; frame_content=[]
  ; frame_x0 = 0.0 ; frame_x1 = 0.0 ; frame_y0 = 0.0 ; frame_y1 = 0.0 }

(** Creates a new frame with the given top-left and bottom-right
    corners, appends this frame as a child of the current frame, and
    moves down to this new frame. *)
let frame ?(contents=[]) x0 y0 x1 y1 (t,cxt)=
  let i=try fst (IntMap.max_binding t.frame_children) with Not_found->(-1) in
  {empty_frame with frame_x0=x0;
    frame_y0=y0;
    frame_x1=x1;
    frame_y1=y1;
    frame_content=contents;
  }, ((i+1,t)::cxt)

let make_page (w, h) zip =
  let (page, zip) = frame 0. 0. w h zip in
  ({page with frame_tags = ["page"]}, zip)

let twocols (t,cxt)=
  let w=t.frame_x1-.t.frame_x0 in
  let sp=w/.40. in
  { t with frame_children=
      IntMap.add 0
        {t with frame_children=IntMap.empty;
          frame_x1=t.frame_x0+.w/.2.-.sp }
        (IntMap.add 1
           {t with frame_children=IntMap.empty;
             frame_x0=t.frame_x0+.w/.2.+.sp }
           t.frame_children)
  },cxt


let next_zipper (t,cxt)=
  match cxt with
      []->None
    | (h,_)::_->
      let z=(frame_up (t,cxt)) in
      let _,_,u=IntMap.split h (fst z).frame_children in
      if IntMap.is_empty u then None else (
        let a,b=IntMap.min_binding u in
        Some (b,(a,fst z)::(snd z))
      )


let doc_frame={
  empty_frame with
    frame_x0= -.infinity;
    frame_y0= -.infinity;
    frame_x1= infinity;
    frame_y1= infinity;
}


let frame_page l=
  let rec last cxt=match cxt with
      [h,t]->(
        let a,_,_=IntMap.split h t.frame_children in
	IntMap.fold (fun _ f n ->
	  if List.mem "not_first_state" f.frame_tags then n else n + 1) a 0
      )
    | _::s->last s
    | []->(-1)
  in
  last (snd l)

let layout_page l=frame_page l.layout


let all_contents frame=
  let rec collect f c=
    IntMap.fold (fun k a m->collect a m) (f.frame_children) (f.frame_content@c)
  in
  collect frame []

(* Helper functions for lines *)

let uselessLine=
  { paragraph=0; lineStart= -1; lineEnd= -1; hyphenStart= -1; hyphenEnd= -1; isFigure=false;
    lastFigure=(-1); height= infinity;paragraph_height= -1; page_line= -1;layout=doc_frame,[];
    min_width=0.;nom_width=0.;max_width=0.;line_y0=infinity;line_y1= -.infinity }

let default_params={ measure=0.;
                     left_margin=0.;
                     local_optimization=0;
                     min_height_before=0.;
                     min_height_after=0.;
                     min_page_before=0;
                     min_page_after=0;
                     not_last_line=false;
                     not_first_line=false;
                     min_lines_before=1;
                     min_lines_after=0;
                     absolute=false
                   }


let lines_eq l0 l1=
  (List.map fst (snd l0.layout) =
      List.map fst (snd l1.layout))
    &&
  l0.height=l1.height

(* Fin des definitions *)

let empty_drawing_box=
    {
      drawing_min_width=0.;
      drawing_nominal_width=0.;
      drawing_max_width=0.;
      drawing_width_fixed = true;
      drawing_adjust_before = false;
      drawing_y0=0.;
      drawing_y1=0.;
      drawing_badness=(fun _->0.);
      drawing_break_badness=0.;
      drawing_states=[];
      drawing_contents=(fun _->[])
    }

let drawing ?(adjust_before=false) ?(vcenter=false) ?(hcenter=false) ?offset:(offset=0.) ?states:(states=[]) cont=
  let states=List.fold_left (fun st0 x->match x with
      States s-> unique (st0@s.states_states)
    | _->st0
  ) states cont
  in
  let (a,b,c,d)=RawContent.bounding_box cont in
  let e = (d -. b) /. 2. in
    {
      drawing_min_width=if hcenter then (c-.a) else c;
      drawing_nominal_width=if hcenter then (c-.a) else c;
      drawing_max_width=if hcenter then (c-.a) else c;
      drawing_width_fixed = true;
      drawing_adjust_before = adjust_before;
      drawing_y0=offset +. (if vcenter then b -. e else 0.0);
      drawing_y1=offset +. (if vcenter then d -. e else d -. b);
      drawing_badness=(fun _->0.);
      drawing_break_badness=0.;
      drawing_states=states;
      drawing_contents=(fun _->List.map (translate
                                           (if hcenter then -.a else 0.0)
                                           (if vcenter then offset -. e else offset)) cont)
    }

let drawing_blit a x0 y0 b=
  let w0=max a.drawing_min_width (x0+.b.drawing_min_width) in
  let w1=max a.drawing_max_width (x0+.b.drawing_max_width) in
    { drawing_min_width = w0;
      drawing_nominal_width = max a.drawing_nominal_width (x0+.b.drawing_nominal_width);
      drawing_max_width = w1;
      drawing_width_fixed = true;
      drawing_adjust_before = false;
      drawing_y0=min a.drawing_y0 (y0+.b.drawing_y0);
      drawing_y1=max a.drawing_y1 (y0+.b.drawing_y1);
      drawing_break_badness=0.;
      drawing_states=unique (a.drawing_states@b.drawing_states);
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


let rec box_width comp=function
    GlyphBox x->Fonts.glyphWidth x.glyph*.x.glyph_size/.1000.
  | Glue x
  | Drawing x->x.drawing_min_width+.comp*.(x.drawing_max_width -. x.drawing_min_width)
  | Kerning x->(box_width comp x.kern_contents) +. x.advance_width
  | Hyphen x->Array.fold_left (fun s x->s+.box_width comp x) 0. x.hyphen_normal
  | _->0.


let rec box_size=function
    GlyphBox x->x.glyph_size
  | Kerning x->box_size x.kern_contents
  | Hyphen x when Array.length x.hyphen_normal>0 -> box_size x.hyphen_normal.(0)
  | _->0.

let rec box_interval=function
    GlyphBox x->let y=Fonts.glyphWidth x.glyph*.x.glyph_size/.1000. in (y,y,y)
  | Glue x
  | Drawing x->x.drawing_min_width, x.drawing_nominal_width, x.drawing_max_width
  | Kerning x->
      let (a,b,c)=box_interval x.kern_contents in
      (a +. x.advance_width, b +. x.advance_width, c+. x.advance_width)
  | Hyphen x->boxes_interval x.hyphen_normal
  | _->(0.,0.,0.)
and boxes_interval boxes=
  let a=ref 0. in let b=ref 0. in let c=ref 0. in
    for i=0 to Array.length boxes-1 do
      let (u,v,w)=box_interval (boxes.(i)) in
        a:= !a+.u;b:= !b+.v;c:= !c+.w
    done;
    (!a,!b,!c)


let rec lower_y x=match x with
    GlyphBox y->
      (let y=Fonts.glyph_y0 y.glyph*.y.glyph_size/.1000. in
       y
      )
  | Glue y
  | Drawing y->y.drawing_y0
  | Kerning y->(lower_y y.kern_contents) +. y.kern_y0
  | Empty->infinity
  | _->0.

let rec upper_y x=match x with
    GlyphBox y->(
      let y=Fonts.glyph_y1 y.glyph*.y.glyph_size/.1000. in
      y
    )
  | Glue y
  | Drawing y->y.drawing_y1
  | Kerning y->(upper_y y.kern_contents) +. y.kern_y0
  | Empty-> -.infinity
  | _-> 0.

let knuth_h_badness w1 w = 100.*.(abs_float (w-.w1)) ** 3.
let glue a b c=
  Glue { drawing_min_width= a;
         drawing_max_width= c;
	 drawing_width_fixed = true;
	 drawing_adjust_before = false;
         drawing_y0=infinity; drawing_y1= -.infinity;
         drawing_nominal_width= b;
         drawing_contents=(fun _->[]);
         drawing_break_badness=0.;
         drawing_states=[];
         drawing_badness=knuth_h_badness b }

let resize_drawing l x=
  { x with
    drawing_min_width= x.drawing_min_width*.l;
    drawing_max_width= x.drawing_max_width*.l;
    drawing_nominal_width= x.drawing_nominal_width*.l;
    drawing_y0=x.drawing_y0*.l;
    drawing_y1=x.drawing_y1*.l;
    drawing_contents=(fun w->List.map (RawContent.resize l) (x.drawing_contents w))
  }

let rec resize l=function
    GlyphBox b -> GlyphBox { b with glyph_size= l*.b.glyph_size }
  | Hyphen x->Hyphen { hyphen_normal=Array.map (resize l) x.hyphen_normal;
                       hyphenated=Array.map (fun (a,b)->Array.map (resize l) a, Array.map (resize l) b) x.hyphenated }
  | Drawing x -> Drawing (resize_drawing l x)
  | Glue x -> Glue { x with
    drawing_min_width= x.drawing_min_width*.l;
    drawing_max_width= x.drawing_max_width*.l;
    drawing_y0=x.drawing_y0*.l;
    drawing_y1=x.drawing_y1*.l;
    drawing_nominal_width= x.drawing_nominal_width*.l;
    drawing_contents=(fun w->List.map (RawContent.resize l) (x.drawing_contents w))
  }
  | Kerning x -> Kerning { advance_width = l*.x.advance_width;
                           advance_height = l*.x.advance_height;
                           kern_x0 = l*.x.kern_x0;
                           kern_y0 = l*.x.kern_y0;
                           kern_contents=resize l x.kern_contents }
  | x->x

let rec translate x y=function
    GlyphBox b -> GlyphBox { b with glyph_x = b.glyph_x +. x; glyph_y = b.glyph_y +. y; glyph_kx=b.glyph_kx+.x; glyph_ky=b.glyph_ky+.y }
  | Hyphen h->Hyphen { hyphen_normal=Array.map (translate x y) h.hyphen_normal;
                       hyphenated=Array.map (fun (a,b)->Array.map (translate x y) a, Array.map (translate x y) b) h.hyphenated }
  | Drawing d -> Drawing { d with
                       drawing_y0=d.drawing_y0 +. y;
                       drawing_y1=d.drawing_y1 +. y;
                       drawing_nominal_width= d.drawing_nominal_width;
                       drawing_contents=(fun w->List.map (RawContent.translate x y) (d.drawing_contents w))
                   }
  | Glue g -> Glue { g with
    drawing_y0=g.drawing_y0 +. y;
    drawing_y1=g.drawing_y1 +. y;
    drawing_contents=(fun w->List.map (RawContent.translate x y) (g.drawing_contents w))
  }
  | Kerning k -> Kerning { k with
                           kern_x0 = k.kern_x0 +. x;
                           kern_y0 = k.kern_y0 +. y;
                           kern_contents=translate x y k.kern_contents }
  | x->x

(* vertically re_kern g with p percent under baseline,
   return the center position to reuse with other symbols *)
let vkern_percent_under' gs p envs st =
  let gs = gs envs st in
  let rec vbox' (sy,mi,sk,ma,nb) gs = match gs with
    | [] -> (sy/.float nb,mi,sk/.float nb,ma)
    | GlyphBox g::gs ->
	let acc =
	  sy +. g.glyph_y,
	  min mi (g.glyph_y +. g.glyph_size/.1000.0*.Fonts.glyph_y0 g.glyph),
	  sk +. g.glyph_ky,
	  max ma (g.glyph_y +.  g.glyph_size/.1000.0*.Fonts.glyph_y1 g.glyph),
	  nb + 1
	in vbox' acc gs
	| _ -> failwith "vkern on non glyph"
  in
  let vbox = vbox' (0.0,max_float,0.0,min_float,0) in
  let y,yl,y0,yh = vbox gs in
  let dy = p *. (yh -. yl) -. (y0 -. yl) in
  let center = (yh +. yl) /. 2.0 -. dy in
  center, List.map (function
      GlyphBox g -> GlyphBox {
	g with
	  glyph_y = g.glyph_y -. dy;
	  glyph_ky = 0.0;
      }
    | _ -> failwith "vkern on non glyph") gs

let vkern_percent_under gs p envs st = snd (vkern_percent_under' gs p envs st)

(* vertically re_kern g to have the center at the given height *)
let vkern_center gs c envs st =
  let gs = gs envs st in
  let rec vbox' (sy,mi,sk,ma,nb) gs = match gs with
    | [] -> (sy/.float nb,mi,sk/.float nb,ma)
    | GlyphBox g::gs ->
	let acc =
	  sy +. g.glyph_y,
	  min mi (g.glyph_y +. g.glyph_size/.1000.0*.Fonts.glyph_y0 g.glyph),
	  sk +. g.glyph_ky,
	  max ma (g.glyph_y +.  g.glyph_size/.1000.0*.Fonts.glyph_y1 g.glyph),
	  nb + 1
	in vbox' acc gs
	| _ -> failwith "vkern on non glyph"
  in
  let vbox = vbox' (0.0,max_float,0.0,min_float,0) in
  let y,yl,y0,yh = vbox gs in
  let dy = (yh +. yl) /. 2.0 -. c in
  List.map (function
      GlyphBox g -> GlyphBox {
	g with
	  glyph_y = g.glyph_y -. dy;
	  glyph_ky = 0.0;
      }
    | _ -> failwith "vkern on non glyph") gs

(* vertically re_kern g by a vertical translation *)
let vkern_translate gs dy envs st =
  let gs = gs envs st in
  List.map (function
      GlyphBox g -> GlyphBox {
	g with
	  glyph_y = g.glyph_y -. dy;
	  glyph_ky = 0.0;
      }
    | _ -> failwith "vkern on non glyph") gs

(* vertically kern g as g' *)
let vkern_as gs gs' envs st =
  let gs = gs envs st in
  let gs' = gs' envs st in
  let rec vbox' (sy,mi,sk,ma,nb) gs = match gs with
    | [] -> (sy/.float nb,mi,sk/.float nb,ma)
    | GlyphBox g::gs ->
	let acc =
	  sy +. g.glyph_y,
	  min mi (g.glyph_y +. g.glyph_size/.1000.0*.Fonts.glyph_y0 g.glyph),
	  sk +. g.glyph_ky,
	  max ma (g.glyph_y +.  g.glyph_size/.1000.0*.Fonts.glyph_y1 g.glyph),
	  nb + 1
	in vbox' acc gs
	| _ -> failwith "vkern on non glyph"
  in
  let vbox = vbox' (0.0,max_float,0.0,min_float,0) in
  let y,yl,y0,yh = vbox gs in
  let y',yl',y0',yh' = vbox gs' in
  let s = (yh' -. yl') /. (yh -. yl) in
  List.map (function
      GlyphBox g -> GlyphBox {
	g with
	  glyph_size = g.glyph_size *. s;
	  glyph_y = g.glyph_y *. s;
	  glyph_ky = y0';
	  glyph_x = g.glyph_x *. s;
	  glyph_kx = g.glyph_kx *. s;
      }
    | _ -> failwith "vkern on non glyph") gs

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

let rec line_layout paragraphs line layout=
  fold_left_line paragraphs (fun l x->match x with
      Layout f->f l
    | _->l
  ) layout line


let rec fold_left f x0 boxes=match boxes with
    []->x0
  | Hyphen h::s->fold_left f x0 (Array.to_list h.hyphen_normal @ s)
  | h::s->fold_left f (f x0 h) s


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




let line_height paragraphs figures node=
  if node.isFigure then
    (figures.(node.lastFigure).drawing_y0,
     figures.(node.lastFigure).drawing_y1)
  else (
  let rec line_height boxes k maxk min_height max_height=
    if k>=maxk then (min_height,max_height) else (
      match boxes.(k) with
          Hyphen x->(
            let a,b=line_height x.hyphen_normal 0 (Array.length x.hyphen_normal) min_height max_height in
              line_height boxes (k+1) maxk a b
          )
        | _->(line_height boxes (k+1) maxk
                (min min_height (lower_y boxes.(k)))
                (max max_height (upper_y boxes.(k))))
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
  )
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
let compression paragraphs (parameters) (line)=comp paragraphs parameters.measure
  line.paragraph line.lineStart line.hyphenStart line.lineEnd line.hyphenEnd



let rec box_to_string = function
  | GlyphBox gl   -> Fonts.glyphContents gl.glyph
  | Kerning k     -> box_to_string k.kern_contents
  | Glue _        -> " "
  | Drawing _     -> "[Drawing]"
  | Hyphen h      -> let bs = Array.to_list h.hyphen_normal in
                     String.concat "" (List.map box_to_string bs)
  | Marker _      -> "[Marker]"
  | BeginFigure _ -> "[BeginFigure]"
  | FlushFigure _ -> "[FlushFigure]"
  | Parameters _  -> "[Parameters]"
  | Layout _      -> "[Layout]"
  | Empty         -> ""

let text_line lines node=
  let rec text i=
    if i>=node.lineEnd then [] else
      box_to_string (lines.(node.paragraph).(i)) :: text (i+1)
  in
    String.concat "" (text node.lineStart)
