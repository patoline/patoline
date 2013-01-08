(*
  Copyright Tom Hirschowitz, Florian Hatat, Pierre-Etienne Meunier,
  Christophe Raffalli and others, 2012.

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

open Util

type frame_tag=
    Column
  | Decoration

type frame={
  frame_children:frame IntMap.t;
  frame_tag:frame_tag;
  frame_x0:float;
  frame_y0:float;
  frame_x1:float;
  frame_y1:float;
}

type frame_zipper=frame*((int*frame) list)

let frame_up (t,cxt)=
  match cxt with
      []->(t,cxt)
    | (i,t')::s->({t' with frame_children=IntMap.add i t t'.frame_children},s)

let rec frame_top (t,cxt)=
  match cxt with
      []->(t,cxt)
    | (i,t')::s->frame_top ({t' with frame_children=IntMap.add i t t'.frame_children},s)


let empty_frame={
  frame_children=IntMap.empty;
  frame_tag=Column;
  frame_x0=0.;
  frame_x1=0.;
  frame_y0=0.;
  frame_y1=0.
}

let make_page (w,h) t=
  let page={empty_frame with
    frame_x0=0.;frame_y0=0.;frame_x1=w;frame_y1=h
  }
  in
  let m=try fst (IntMap.max_binding (fst t).frame_children) with Not_found-> -1 in
  (page, (m+1,(fst t))::(snd t))


let frame x0 y0 x1 y1 (t,cxt)=
  let i=try fst (IntMap.max_binding t.frame_children) with Not_found->(-1) in
  {empty_frame with frame_x0=x0;
    frame_y0=y0;
    frame_x1=x1;
    frame_y1=y1
  }, ((i+1,t)::cxt)

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

type line= {
  paragraph:int;                        (** L'indice du paragraphe dans le tableau *)
  lastFigure:int;                       (** La dernière figure placée, initialement -1 *)
  lineStart:int;                        (** Le numéro de la boite de début dans le paragraphe *)
  lineEnd:int;                          (** Le numéro de la boite suivant la dernière boite de la ligne, ou, si la ligne est césurée, le numéro de la boite contenant la césure *)
  hyphenStart:int;
  hyphenEnd:int;
  isFigure:bool;
  layout:frame_zipper;
  height:float;
  paragraph_height:int;
  page_line:int;
  min_width:float;
  nom_width:float;
  max_width:float;
  line_y0:float;
  line_y1:float
}

let uselessLine=
  { paragraph=0; lineStart= -1; lineEnd= -1; hyphenStart= -1; hyphenEnd= -1; isFigure=false;
    lastFigure=(-1); height= infinity;paragraph_height= -1; page_line= -1;layout=doc_frame,[];
    min_width=0.;nom_width=0.;max_width=0.;line_y0=infinity;line_y1= -.infinity }

let sprint_linef l=
  Printf.sprintf "{ paragraph=%d; lineStart=%d; lineEnd=%d; hyphenStart=%d; hyphenEnd=%d; lastFigure=%d; height=%f; isFigure=%b }"
    l.paragraph l.lineStart l.lineEnd l.hyphenStart l.hyphenEnd l.lastFigure l.height
    l.isFigure

let print_linef out l=Printf.fprintf out "%s\n" (sprint_linef l)
let print_line l=print_linef stderr l

let frame_page l=
  let rec last cxt=match cxt with
      [h,t]->(
        let a,_,_=IntMap.split h t.frame_children in
        IntMap.cardinal a
      )
    | _::s->last s
    | []->(-1)
  in
  last (snd l)

let page l=frame_page l.layout

type parameters={ measure:float;
                  left_margin:float;
                  local_optimization:int;
                  min_height_before:float;
                  min_height_after:float;
                  min_page_before:int;
                  min_page_after:int;
                  not_last_line:bool;
                  not_first_line:bool;
                  min_lines_before:int;
                  min_lines_after:int;
                  absolute:bool
                }

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


type placed_line=
    { line_params:parameters;
      line:line }
