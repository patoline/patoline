type lineCap=Butt_cap | Round_cap | Proj_square_cap
type lineJoin=Miter_join | Round_join | Bevel_join

type rgb={ red:float; green:float; blue:float }
type color=
    RGB of rgb
let rgb a b c=RGB { red=a;green=b;blue=c }

let mix x a b=match a,b with
    RGB ra, RGB rb->RGB { red=(x*.ra.red+.(100.-.x)*.rb.red)/.100.;
                          green=(x*.ra.green+.(100.-.x)*.rb.green)/.100.;
                          blue=(x*.ra.blue+.(100.-.x)*.rb.blue)/.100.
                        }

let black=RGB { red=0.;green=0.;blue=0. }
let white=RGB{red=1.;green=1.;blue=1.}
let black=RGB{red=0.;green=0.;blue=0.}
let blue=RGB{red=0.;green=0.;blue=1.}
let green=RGB{red=0.;green=1.;blue=0.}
let red=RGB{red=1.;green=0.;blue=0.}
let yellow=RGB{red=1.;green=1.;blue=0.}
let gray=mix 50. white black


type metadata=
    Contributor
  | Coverage
  | Creator
  | Date
  | Description
  | Format
  | Identifier
  | Language
  | Publisher
  | Relation
  | Rights
  | Source
  | Subject
  | Title
  | Type



type path_parameters= {
  close:bool;
  strokingColor:color option;
  fillColor:color option;
  lineCap:lineCap;
  lineJoin:lineJoin;
  lineWidth:float;
  dashPattern:float list
}

let default= { close=false;strokingColor=Some black;fillColor=None;
               lineCap=Butt_cap; lineJoin=Miter_join; lineWidth=0.1;
               dashPattern=[] }


type glyph={ glyph_x:float; glyph_y:float; glyph_color: color; glyph_size:float;
             glyph:Fonts.glyph }

type link= { mutable link_x0:float;mutable link_y0:float;mutable link_x1:float;mutable link_y1:float;
             uri:string;
             dest_page:int; dest_x:float; dest_y:float }

type image= { image_file:string; image_x:float; image_y:float; image_height:float;image_width:float }

type contents=
    Glyph of glyph
  | Path of path_parameters * (Bezier.curve array list)
  | Link of link
  | Image of image
  | States of contents list*Util.IntSet.t

let rec translate x y=function
    Glyph g->Glyph { g with glyph_x=g.glyph_x+.x; glyph_y=g.glyph_y+.y }
  | Path (a,b)->Path (a, List.map (Array.map (fun (u,v)->(Array.map (fun x0->x0+.x) u, Array.map (fun y0->y0+.y) v))) b)
  | Link l -> Link { l with link_x0=l.link_x0+.x; link_y0=l.link_y0+.y;
                       link_x1=l.link_x1+.x; link_y1=l.link_y1+.y }
  | Image i->Image { i with image_x=i.image_x+.x;image_y=i.image_y+.y }
  | States (a,b)->States ((List.map (translate x y) a), b)

let rec resize alpha=function
    Glyph g->Glyph { g with glyph_x=g.glyph_x*.alpha; glyph_y=g.glyph_y*.alpha; glyph_size=g.glyph_size*.alpha }
  | Path (a,b)->Path ( { a with lineWidth=a.lineWidth*.alpha },
                       List.map (Array.map (fun (u,v)->(Array.map (fun x0->x0*.alpha) u, Array.map (fun y0->y0*.alpha) v))) b)
  | Link l -> Link { l with link_x0=l.link_x0*.alpha; link_y0=l.link_y0*.alpha;
                       link_x1=l.link_x1*.alpha; link_y1=l.link_y1*.alpha }
  | Image i->Image { i with image_width=i.image_width*.alpha;
                       image_height=i.image_height*.alpha }
  | States (a,b)->States ((List.map (resize alpha) a), b)

type bounding_box_opt = {
  ignore_negative_abcisse : bool;
  ignore_after_glyphWidth : bool;
  ignore_under_base_line : bool}

let bounding_box_opt opt l=
  let rec bb x0 y0 x1 y1=function
      []->(x0,y0,x1,y1)
    | Glyph g::s->(
        let x0'=g.glyph_x +. 
	  (if opt.ignore_negative_abcisse then 0.0 else Fonts.glyph_x0 g.glyph) 
          *. g.glyph_size/.1000. in
        let x1'=g.glyph_x +.
	  (if opt.ignore_after_glyphWidth then Fonts.glyphWidth g.glyph else Fonts.glyph_x1 g.glyph)
	  *. g.glyph_size /.1000. in
        let y0'=g.glyph_y +. 
	  (if opt.ignore_under_base_line then 0.0 else Fonts.glyph_y0 g.glyph)
	  *. g.glyph_size /.1000. in
        let y1'=g.glyph_y +. g.glyph_size*.Fonts.glyph_y1 g.glyph/.1000. in
          bb (min x0 x0') (min y0 y0') (max x1 x1') (max y1 y1') s
      )
    | Path (_,ps)::s->(
        let x0'=ref x0 in
        let y0'=ref y0 in
        let x1'=ref x1 in
        let y1'=ref y1 in
          List.iter (fun p->
                       for i=0 to Array.length p-1 do
                         let (xa,ya),(xb,yb)=Bezier.bounding_box p.(i) in
                           x0' := min !x0' xa;
                           y0' := min !y0' ya;
                           x1' := max !x1' xb;
                           y1' := max !y1' yb;
                       done) ps;
          bb !x0' !y0' !x1' !y1' s
      )
    | Image i::s->
        bb (min x0 i.image_x) (min y0 i.image_y)
          (max x1 (i.image_x+.i.image_width))
          (max y1 (i.image_y+.i.image_height)) s

    | States (a,b)::s->bb x0 y0 x1 y1 (a@s)

    | _::s -> bb x0 y0 x1 y1 s
  in
    bb infinity infinity (-.infinity) (-.infinity) l

let bounding_box =  bounding_box_opt {
  ignore_negative_abcisse = true;
  ignore_after_glyphWidth = true;
  ignore_under_base_line = false}

let bounding_box_kerning =  bounding_box_opt {
  ignore_negative_abcisse = true;
  ignore_after_glyphWidth = true;
  ignore_under_base_line = true}

let bounding_box_full =  bounding_box_opt {
  ignore_negative_abcisse = false;
  ignore_after_glyphWidth = false;
  ignore_under_base_line = false}

let rectangle (xa,ya) (xb,yb)=
  [|[|xa;xa|],[|ya;yb|];
    [|xa;xb|],[|yb;yb|];
    [|xb;xb|],[|yb;ya|];
    [|xb;xa|],[|ya;ya|];
  |]

let circle r=
  let lambda=r*.4.*.(sqrt 2.-.1.)/.3. in
    [| [|-.r;-.r;-.lambda;0.|], [|0.;lambda;r;r|];
       [|0.;lambda;r;r|], [|r;r;lambda;0.|];
       [|r;r;lambda;0.|], [|0.;-.lambda;-.r;-.r|];
       [|0.;-.lambda;-.r;-.r|], [|-.r;-.r;-.lambda;0.|] |]
type structure= { mutable name:string;
                  mutable metadata : (metadata*string) list;
		  mutable displayname:contents list;
                  mutable page:int;
                  mutable struct_x:float;
                  mutable struct_y:float;
                  mutable substructures:structure array }

let print_structure s = 
  let rec fn lvl s =
    for i = 0 to lvl do 
      Printf.printf " ";
    done;
    Printf.printf "%s\n" s.name;
    Array.iter (fn (lvl+1)) s.substructures
  in
  fn 0 s;
  flush stdout
