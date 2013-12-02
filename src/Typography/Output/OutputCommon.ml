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
type lineCap=Butt_cap | Round_cap | Proj_square_cap
type lineJoin=Miter_join | Round_join | Bevel_join

type rgb={ red:float; green:float; blue:float }
type color=
    RGB of rgb
let rgb a b c=RGB { red=a;green=b;blue=c }

let mix x a b=match a,b with
    RGB ra, RGB rb->RGB { red=(x*.ra.red+.(1.-.x)*.rb.red);
                          green=(x*.ra.green+.(1.-.x)*.rb.green);
                          blue=(x*.ra.blue+.(1.-.x)*.rb.blue)
                        }

let black=RGB { red=0.;green=0.;blue=0. }
let white=RGB{red=1.;green=1.;blue=1.}
let blue=RGB{red=0.;green=0.;blue=1.}
let green=RGB{red=0.;green=1.;blue=0.}
let red=RGB{red=1.;green=0.;blue=0.}
let orange=RGB{red=1.;green=0.5;blue=0.}
let purple=RGB{red=0.5;green=0.;blue=1.}
let pink=RGB{red=1.;green=0.;blue=0.5}
let yellow=RGB{red=1.;green=1.;blue=0.}
let gray=mix 0.5 white black
let grey=mix 0.5 white black


let hsv h s v=
  let h'=h/.60. in
  let c=v*.s in
  let h'mod2=
    h'-.(float_of_int (2*(int_of_float (h'/.2.))))
  in
  let x=c*.(1.-.(abs_float (h'mod2-.1.))) in
  if h'<1. then rgb c x 0. else
  if h'<2. then rgb x c 0. else
  if h'<3. then rgb 0. c x else
  if h'<4. then rgb 0. x c else
  if h'<5. then rgb x 0. c else
  if h'<6. then rgb c 0. x else
    rgb 0. 0. 0.


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
  path_order:int;
  close:bool;
  strokingColor:color option;
  fillColor:color option;
  lineCap:lineCap;
  lineJoin:lineJoin;
  lineWidth:float;
  dashPattern:float list
}
let default= { path_order=0;close=false;strokingColor=Some black;fillColor=None;
               lineCap=Butt_cap; lineJoin=Miter_join; lineWidth=0.1;
               dashPattern=[] }

type glyph={ glyph_x:float; glyph_kx:float; glyph_y:float; glyph_ky:float; glyph_order:int; glyph_color: color; glyph_size:float;
             glyph:Fonts.glyph }
type image= { image_file:string; image_x:float; image_y:float; image_order:int; image_height:float;image_width:float;image_pixel_width:int;image_pixel_height:int }

type video= { video_file:string; video_x:float; video_y:float; video_order:int; video_height:float;video_width:float;video_pixel_width:int;video_pixel_height:int }
type link= { mutable link_x0:float;mutable link_y0:float;mutable link_x1:float;mutable link_y1:float;
             mutable link_closed:bool;
             link_order:int;
             uri:string;is_internal:bool;
             dest_page:int; dest_x:float; dest_y:float;
             mutable link_contents:raw list}
and states={
  states_contents:raw list;
  states_states:Util.IntSet.t;
  states_order:int
}

and raw=
    Glyph of glyph
  | Path of path_parameters * (Bezier.curve array list)
  | Link of link
  | Image of image
  | Video of video
  | States of states
  | Animation of raw list * string array * (float -> float array) * (float array -> raw list)

let rec translate x y=function
    Glyph g->Glyph { g with glyph_x=g.glyph_x+.x; glyph_y=g.glyph_y+.y; glyph_kx=g.glyph_kx+.x; glyph_ky=g.glyph_ky+.y  }
  | Path (a,b)->Path (a, List.map (Array.map (fun (u,v)->(Array.map (fun x0->x0+.x) u, Array.map (fun y0->y0+.y) v))) b)
  | Link l -> Link { l with
    link_x0=l.link_x0+.x; link_y0=l.link_y0+.y;
    link_x1=l.link_x1+.x; link_y1=l.link_y1+.y;
    link_contents=List.map (translate x y) l.link_contents
  }
  | Image i->Image { i with image_x=i.image_x+.x;image_y=i.image_y+.y }
  | Video i->Video { i with video_x=i.video_x+.x;video_y=i.video_y+.y }
  | States s->States { s with states_contents=List.map (translate x y) s.states_contents }
  | Animation (r, names, ft, fr) -> Animation(List.map (translate x y) r, names, ft, (fun a -> List.map (translate x y) (fr a)))

let rec resize alpha=function
    Glyph g->Glyph { g with glyph_x=g.glyph_x*.alpha; glyph_y=g.glyph_y*.alpha; glyph_kx=g.glyph_kx*.alpha; glyph_ky=g.glyph_ky*.alpha;  
      glyph_size=g.glyph_size*.alpha }
  | Path (a,b)->Path ( { a with lineWidth=a.lineWidth*.alpha },
                       List.map (Array.map (fun (u,v)->(Array.map (fun x0->x0*.alpha) u, Array.map (fun y0->y0*.alpha) v))) b)
  | Link l -> Link { l with
    link_x0=l.link_x0*.alpha; link_y0=l.link_y0*.alpha;
    link_x1=l.link_x1*.alpha; link_y1=l.link_y1*.alpha;
    link_contents=List.map (resize alpha) l.link_contents
  }
  | Image i->Image { i with image_width=i.image_width*.alpha;
                       image_height=i.image_height*.alpha }
  | Video i->Video { i with video_width=i.video_width*.alpha;
                       video_height=i.video_height*.alpha }
  | States s->States { s with states_contents=List.map (resize alpha) s.states_contents }
  | Animation (r, names, ft, fr) -> Animation(List.map (resize alpha) r, names, ft, (fun a -> List.map (resize alpha) (fr a)))


type bounding_box_opt = {
  ignore_negative_abcisse : bool;
  ignore_after_glyphWidth : bool;
  ignore_under_base_line : bool}

let rec print_raw r=match r with
  Glyph g->Printf.fprintf stderr "Glyph %s (%f,%f) size %f\n" (Fonts.glyphNumber g.glyph).FTypes.glyph_utf8 g.glyph_x g.glyph_y g.glyph_size
  | Path (_,ps)->(

    let c=String.concat "," (List.map (fun p->
      "["^(String.concat ";"
             (Array.to_list
                (Array.map (fun (x,y)->
                  "[|"^(String.concat ";" (List.map string_of_float (Array.to_list x)))
                  ^"|],[|"
                  ^(String.concat ";" (List.map string_of_float (Array.to_list y)))
                  ^"|]"
                 ) p)
             )
      )^"]"
    ) ps)
    in
    Printf.fprintf stderr "Path [%s]\n" c;
  )
  | Image i->
    Printf.fprintf stderr "Image (%f,%f) (%f,%f)\n" i.image_x i.image_y (i.image_x+.i.image_width)
      (i.image_y+.i.image_height)
  | Video i->
    Printf.fprintf stderr "Video (%f,%f) (%f,%f)\n" i.video_x i.video_y (i.video_x+.i.video_width)
      (i.video_y+.i.video_height)

  | States a->List.iter print_raw a.states_contents
  | Link l->(Printf.fprintf stderr "Link [";List.iter print_raw l.link_contents;Printf.fprintf stderr "]\n")
  | Animation(r,_,_,_)->List.iter print_raw r


let bounding_box_opt opt l=
  let rec bb x0 y0 x1 y1=function
      []->(x0,y0,x1,y1)
    | Glyph g::s->(
      let x0'=
	if opt.ignore_negative_abcisse then g.glyph_kx else g.glyph_x +. Fonts.glyph_x0 g.glyph *. g.glyph_size/.1000.
      in
      let x1'=g.glyph_x +.
	(if opt.ignore_after_glyphWidth then Fonts.glyphWidth g.glyph else Fonts.glyph_x1 g.glyph) *. g.glyph_size /.1000.
      in
      let y0'=
	if opt.ignore_under_base_line then g.glyph_ky else g.glyph_y +. Fonts.glyph_y0 g.glyph *. g.glyph_size /.1000.
      in
      let y1'=
	g.glyph_y +.Fonts.glyph_y1 g.glyph *.  g.glyph_size/.1000.
      in
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
    | Video i::s->
        bb (min x0 i.video_x) (min y0 i.video_y)
          (max x1 (i.video_x+.i.video_width))
          (max y1 (i.video_y+.i.video_height)) s

    | States a::s->bb x0 y0 x1 y1 (a.states_contents@s)
    | Link l::s->bb x0 y0 x1 y1 (l.link_contents@s)
    | Animation(r,_,_,_)::s -> bb x0 y0 x1 y1 (r@s)
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

let line (xa,ya) (xb,yb)=
  [|xa;xb|],[|ya;yb|]

let circle r=
  let lambda=r*.4.*.(sqrt 2.-.1.)/.3. in
    [| [|-.r;-.r;-.lambda;0.|], [|0.;lambda;r;r|];
       [|0.;lambda;r;r|], [|r;r;lambda;0.|];
       [|r;r;lambda;0.|], [|0.;-.lambda;-.r;-.r|];
       [|0.;-.lambda;-.r;-.r|], [|-.r;-.r;-.lambda;0.|] |]


type structure= { mutable name:string;
                  mutable metadata : (metadata*string) list;
		  mutable displayname:raw list;
                  mutable tags:(string*string) list;
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

let output_to_prime (output:(?structure:structure -> 'a array -> 'b -> 'c))
    ?(structure:structure={name="";displayname=[];metadata=[];tags=[];
			   page= -1;struct_x=0.;struct_y=0.;substructures=[||]})
    pages fileName =
  let l = ref [] in
  Array.iter (fun ps -> Array.iter (fun p -> l:=p::!l) ps) pages;
  output ~structure (Array.of_list (List.rev !l)) fileName

let output_from_prime (output:(?structure:structure -> 'a array -> 'b -> 'c))
    ?(structure:structure={name="";displayname=[];metadata=[];tags=[];
			   page= -1;struct_x=0.;struct_y=0.;substructures=[||]})
    pages fileName =
  output ~structure (Array.map (fun x -> [|x|]) pages) fileName

let rec in_order i x=match x with
    Glyph g->Glyph { g with glyph_order=i }
  | Path (p,t)->Path ({p with path_order=i},t)
  | Link l->Link { l with link_order=i }
  | Image a->Image { a with image_order=i }
  | Video a->Video { a with video_order=i }
  | States s->States { s with states_order=i }
  | Animation(r,names,ft,fr) ->Animation(List.map (in_order i) r,names,ft,(fun a -> List.map (in_order i) (fr a))) 

let rec drawing_order x=match x with
    Glyph g->g.glyph_order
  | Path (p,_)->p.path_order
  | Link l->l.link_order
  | Image i->i.image_order
  | Video i->i.video_order
  | States s->s.states_order
  | Animation(r,_,_,_) -> List.fold_left (fun acc r -> min acc (drawing_order r)) max_int r

let drawing_sort l=
  let rec make_list t acc=match t with
      []->acc
    | States h::s->
      let m=List.fold_left (fun m x->
        let l=try Util.IntMap.find (drawing_order x) m with Not_found->[] in
        Util.IntMap.add (drawing_order x) (x::l) m
      ) Util.IntMap.empty h.states_contents
      in
      make_list s (Util.IntMap.fold (fun k a l->
        States { h with states_order=k;states_contents=a }::l)
                     m acc)
    | h::s->make_list s (h::acc)
  in
  List.sort (fun a b->compare (drawing_order a) (drawing_order b)) (make_list l [])

open Util
let sort_raw l=
  let x=List.fold_left (fun m x->
    let m'=try IntMap.find (drawing_order x) m with Not_found->[] in
    IntMap.add (drawing_order x) (x::m') m
  ) IntMap.empty l
  in
  let comp a b=match a,b with
      Glyph ga,Glyph gb->if ga.glyph_y=gb.glyph_y then compare ga.glyph_x gb.glyph_x
        else compare gb.glyph_y ga.glyph_y
    | Glyph ga,_-> -1
    | _,Glyph gb->1
    | _->0
  in
  let subsort a=match a with
      Link l->Link { l with link_contents=List.sort comp l.link_contents }
    | b->b
  in
  IntMap.fold (fun _ a x->x@a) (IntMap.map (fun l->(List.sort comp (List.map subsort l))) x) []
