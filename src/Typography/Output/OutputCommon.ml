type lineCap=Butt_cap | Round_cap | Proj_square_cap
type lineJoin=Miter_join | Round_join | Bevel_join

type rgb={ red:float; green:float; blue:float }
type color=
    RGB of rgb
let rgb a b c=RGB { red=a;green=b;blue=c }

let black=RGB { red=0.;green=0.;blue=0. }

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
               lineCap=Butt_cap; lineJoin=Miter_join; lineWidth=1.;
               dashPattern=[] }


type glyph={ glyph_x:float; glyph_y:float; glyph_color: color; glyph_size:float;
             glyph:Fonts.glyph }

type link= { link_x0:float;link_y0:float;link_x1:float;link_y1:float;
             dest_page:int; dest_x:float; dest_y:float }

type contents=
    Glyph of glyph
  | Path of path_parameters * (Bezier.curve array list)
  | Link of link

let translate x y=function
    Glyph g->Glyph { g with glyph_x=g.glyph_x+.x; glyph_y=g.glyph_y+.y }
  | Path (a,b)->Path (a, List.map (Array.map (fun (u,v)->(Array.map (fun x0->x0+.x) u, Array.map (fun y0->y0+.y) v))) b)
  | Link l -> Link { l with link_x0=l.link_x0+.x; link_y0=l.link_y0+.y;
                       link_x1=l.link_x1+.x; link_y1=l.link_y1+.y }

let resize alpha=function
    Glyph g->Glyph { g with glyph_x=g.glyph_x*.alpha; glyph_y=g.glyph_y*.alpha; glyph_size=g.glyph_size*.alpha }
  | Path (a,b)->Path ( { a with lineWidth=a.lineWidth*.alpha },
                       List.map (Array.map (fun (u,v)->(Array.map (fun x0->x0*.alpha) u, Array.map (fun y0->y0*.alpha) v))) b)
  | Link l -> Link { l with link_x0=l.link_x0*.alpha; link_y0=l.link_y0*.alpha;
                       link_x1=l.link_x1*.alpha; link_y1=l.link_y1*.alpha }


let bounding_box l=
  let rec bb x0 y0 x1 y1=function
      []->(x0,y0,x1,y1)
    | Glyph g::s->(
        let x0'=g.glyph_x in
        let x1'=g.glyph_x +. g.glyph_size*.Fonts.glyphWidth g.glyph/.1000. in
        let y0'=g.glyph_y +. g.glyph_size*.Fonts.glyph_y0 g.glyph/.1000. in
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
    | _::s -> bb x0 y0 x1 y1 s
  in
    bb infinity infinity (-.infinity) (-.infinity) l


let rectangle (xa,ya) (xb,yb)=
  [|[|xa;xa|],[|ya;yb|];
    [|xa;xb|],[|yb;yb|];
    [|xb;xb|],[|yb;ya|]|]

let circle r=
  let lambda=r*.4.*.(sqrt 2.-.1.)/.3. in
    [| [|0.;0.;r-.lambda;r|], [|r;r+.lambda;2.*.r;2.*.r|];
       [|r;r+.lambda;2.*.r;2.*.r|], [|2.*.r;2.*.r;r+.lambda;r|];
       [|2.*.r;2.*.r;r+.lambda;r|], [|r;r-.lambda;0.;0.|];
       [|r;r-.lambda;0.;0.|], [|0.;0.;r-.lambda;r|] |]
type structure= { mutable name:string;
		  mutable displayname:contents list;
                  mutable page:int;
                  mutable struct_x:float;
                  mutable struct_y:float;
                  mutable substructures:structure array }
