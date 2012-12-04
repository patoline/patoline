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
open Typography
open Typography.Fonts
open Typography.Fonts.FTypes
open Typography.Document
open Typography.Util
open Typography.Box
open Typography.Line
open CamomileLibrary
open Printf

let id x=x
let emph x=toggleItalic x

module Format=functor (D:DocumentStructure)->struct

  module Default=DefaultFormat.Format(D)
  include Default

  let subject_text: content list ref = ref (toggleItalic [tT "Subject:";tT" "]) 

  let rec lines s=try
    let idx=String.index s '\n' in
      String.sub s 0 idx::lines (String.sub s (idx+1) (String.length s-idx-1))
  with
      Not_found->if s="" then [] else [s]
  let rec repeat x n=if n<=0 then [] else x::(repeat x (n-1))

  let postprocess_tree tree=
    fst (
      top (
        newChildBefore (tree,[])
          (fst
             (paragraph
                [ bB (fun env->
                       let w=env.normalMeasure/.2. in
                       let sender=match tree with
                           Node n->(try List.assoc "sender" n.node_tags with Not_found->"")
                         | _ -> ""
                       in
                       let recipient=match tree with
                           Node n->(try List.assoc "receiver" n.node_tags with Not_found->"")
                         | _ -> ""
                       in
                       let lines_recipient=lines recipient in
                       let lines_sender=
                         let l=lines sender in
                           repeat " " (List.length lines_recipient-List.length l)@l
                       in
                       let pars_sender=node (List.map (fun l->paragraph [tT l]) (lines_sender)) in
                       let pars_recip=node (List.map (fun l->paragraph [tT l]) (lines_recipient)) in
                       let minip_sender=(Default.minipage { env with
                                                              normalMeasure=w;
                                                              par_indent=[]} pars_sender).(0) in
                       let minip_recip=(Default.minipage { env with normalMeasure=w;
                                                             par_indent=[] } pars_recip).(0) in
                       let x0=min 0. (env.normalMeasure/.2.-.minip_sender.drawing_nominal_width) in
                       let x1=max (env.normalMeasure/.2.)
                         (env.normalMeasure-.minip_recip.drawing_nominal_width) in
                       let y0=round_float ((minip_recip.drawing_y0-.minip_sender.drawing_y0)
                                           /.env.normalLead)*.env.normalLead in
                       let y1=0. in
                       let c0=(minip_sender.drawing_contents minip_sender.drawing_nominal_width) in
                       let c1=(minip_recip.drawing_contents minip_recip.drawing_nominal_width) in
                       let contents=
                         (List.map (OutputCommon.translate x0 y0) c0)@
                           (List.map (OutputCommon.translate x1 y1) c1)
                       in
                         [Drawing {
                            drawing_min_width= env.normalMeasure;
                            drawing_max_width= env.normalMeasure;
                            drawing_nominal_width= env.normalMeasure;
                            drawing_y0= min (minip_sender.drawing_y0+.y0) (minip_recip.drawing_y0+.y1);
                            drawing_y1= max (minip_sender.drawing_y1+.y0) (minip_recip.drawing_y1+.y1);
                            drawing_badness=(fun _->0.);
                            drawing_contents=(fun _->contents)
                          }]
                    )]
             ))))

  let sender x=
    D.structure:=follow
      (tag (fst (top !D.structure)) ["sender",x], [])
      (List.map fst (snd !D.structure))

  let receiver x=
    D.structure:=follow
      (tag (fst (top !D.structure)) ["receiver",x], [])
      (List.map fst (snd !D.structure))

  open Unix
  let lieu_date_text  = ref (fun lieu x -> 
    (Printf.sprintf "%s, le %d/%d/%d" lieu x.tm_mday (x.tm_mon + 1) (1900 + x.tm_year)))


  let lieu_date lieu x =
      newPar D.structure Complete.normal ragged_right
        ((vspaceBefore 10.) @ [tT (!lieu_date_text lieu x)])
  let dear x=(vspaceBefore 13.)@(vspaceAfter 3.)@x
  let subject x= !subject_text @ x

end


open OutputPaper
open OutputCommon

module Output=functor(M:Driver)->struct
  let output structure paragraphs (figures:drawingBox array) env (opt_pages:(parameters*line) list array) file=
    let positions=Array.make (Array.length paragraphs) (0,0.,0.) in
    let pages=Array.make (Array.length opt_pages) { pageFormat=a4 ; pageContents=[] } in
      for i=0 to Array.length opt_pages-1 do pages.(i)<- { pageFormat=a4 ; pageContents=[] } done;
    let par=ref (-1) in
    let crosslinks=ref [] in (* (page, link, destination) *)
    let crosslink_opened=ref false in
    let destinations=ref StrMap.empty in
    let urilinks=ref None in
    let draw_page i p=
      let page=pages.(i) in
      let footnotes=ref [] in
      let footnote_y=ref (-.infinity) in
        List.iter (
          fun (param,line)->
            let y=270.0-.line.height in

              if line.isFigure then (
                let fig=figures.(line.lastFigure) in
                  page.pageContents<- (List.map (translate param.left_margin (y-.fig.drawing_y1))
                                         (fig.drawing_contents fig.drawing_nominal_width))
                  @ page.pageContents;

              ) else if line.paragraph<Array.length paragraphs then (

                if line.paragraph<> !par then (
                  par:=line.paragraph;
                  positions.(!par)<-(i,0., y +. phi*.snd (line_height paragraphs figures line))
                );

                let comp=compression paragraphs param line in
                let rec draw_box x y box=
                  let lowy=y+.lower_y box in
                  let uppy=y+.upper_y box in
                    (match !urilinks with
                         None->()
                       | Some h->(
                           h.link_y0<-min h.link_y0 lowy;
                           h.link_y1<-max h.link_y1 uppy
                         ));
                    if !crosslink_opened then
                      (match !crosslinks with
                           []->()
                         | (_,h,_)::_->(
                             h.link_y0<-min h.link_y0 lowy;
                             h.link_y1<-max h.link_y1 uppy
                           ));
                  match box with
                      Kerning kbox ->(
                        let fact=(box_size kbox.kern_contents/.1000.) in
                        let w=draw_box (x+.kbox.kern_x0*.fact) (y+.kbox.kern_y0*.fact) kbox.kern_contents in
                          w+.kbox.advance_width*.fact
                      )
                    | Hyphen h->(
                        (Array.fold_left (fun x' box->
                                            let w=draw_box (x+.x') y box in
                                              x'+.w) 0. h.hyphen_normal)
                      )
                    | GlyphBox a->(
                        page.pageContents<-
                          (OutputCommon.Glyph { a with glyph_x=a.glyph_x+.x;glyph_y=a.glyph_y+.y })
                        :: page.pageContents;
                        a.glyph_size*.Fonts.glyphWidth a.glyph/.1000.
                      )
                    | Glue g
                    | Drawing g ->(
                        let w=g.drawing_min_width+.comp*.(g.drawing_max_width-.g.drawing_min_width) in
                          page.pageContents<- (List.map (translate x y) (g.drawing_contents w)) @ page.pageContents;
                          (* page.pageContents<- Path ({OutputCommon.default with close=true;lineWidth=0.1 }, [rectangle (x,y+.g.drawing_y0) (x+.w,y+.g.drawing_y1)]) :: page.pageContents; *)
                          w
                      )
                      | User (BeginURILink l)->(
                        let link={ link_x0=x;link_y0=y;link_x1=x;link_y1=y;uri=l;
                                   link_order=0;
                                   dest_page=0;dest_x=0.;dest_y=0.;is_internal=false;
                                   link_contents=[] }
                        in
                        urilinks:=Some link;
                        page.pageContents<-Link link::page.pageContents;
                        0.
                      )
                      | User (BeginLink l)->(
                        let link={ link_x0=x;link_y0=y;link_x1=x;link_y1=y;uri="";
                                   link_order=0;
                                   dest_page=0;dest_x=0.;dest_y=0.;is_internal=true;
                                   link_contents=[]
                                 }
                        in
                        crosslinks:=(i, link, l) :: !crosslinks;
                        page.pageContents<-Link link::page.pageContents;
                        crosslink_opened:=true;
                        0.
                      )
                      | User (Label l)->(
                        let y0,y1=line_height paragraphs figures line in
                        destinations:=StrMap.add l (i,param.left_margin,y+.y0,y+.y1) !destinations;
                        0.
                      )
                      | User EndLink->(
                        let rec link_contents u l=match l with
                            []->[]
                          | (Link h)::s->(Link { h with link_contents=List.rev u })::s
                          | h::s->link_contents (h::u) s
                        in
                        page.pageContents<-link_contents [] page.pageContents;
                        (match !urilinks with
                            None->(
                              match !crosslinks with
                                  []->()
                                | (_,h,_)::s->crosslink_opened:=false; h.link_x1<-x
                            )
                          | Some h->(
                            h.link_x1<-x;
                            page.pageContents<-Link h::page.pageContents;
                            urilinks:=None;
                          )
                        );
                        0.
                      )
                    | User (Footnote (_,g))->(
                        footnotes:= g::(!footnotes);
                        footnote_y:=max !footnote_y (270.-.param.page_height);
                        0.
                      )
                    | b->box_width comp b
                in
                  urilinks:=(match !urilinks with
                                 None->None
                               | Some h->
                                   page.pageContents<-Link h::page.pageContents;
                                   Some { h with link_x0=param.left_margin;link_x1=param.left_margin;
                                            link_y0=y;link_y1=y });
                  if !crosslink_opened then
                    crosslinks:=(match !crosslinks with
                                     []->[]
                                   | (a,h,c)::s->
                                       (a, { h with link_x0=param.left_margin;link_x1=param.left_margin;
                                               link_y0=y;link_y1=y }, c)::(a,h,c)::s);
                  let x1=fold_left_line paragraphs (fun x b->x+.draw_box x y b) param.left_margin line in
                    (match !urilinks with
                         None->()
                       | Some h->h.link_x1<-x1);
                    if !crosslink_opened then
                      (match !crosslinks with
                           []->()
                         | (_,h,_)::_->h.link_x1<-x1)
              )
        ) p;
        (match !urilinks with
             None->()
           | Some h->page.pageContents<-Link h::page.pageContents; urilinks:=None);
        ignore (
          List.fold_left (
            fun y footnote->
              page.pageContents<- (List.map (translate (env.normalLeftMargin) (y-.footnote.drawing_y1-.env.footnote_y))
                                     (footnote.drawing_contents footnote.drawing_nominal_width)) @ page.pageContents;
              y-.(footnote.drawing_y1-.footnote.drawing_y0)
          ) !footnote_y !footnotes
        );
        if !footnotes<>[] then (
          page.pageContents<- (Path ({OutputCommon.default with lineWidth=0.01 }, [ [| [| env.normalLeftMargin;
                                                                    env.normalLeftMargin+.env.normalMeasure*.(2.-.phi) |],
                                                                 [| !footnote_y-.env.footnote_y;
                                                                    !footnote_y-.env.footnote_y |] |] ]))::page.pageContents
        );
        (* let pnum=glyph_of_string env.substitutions env.positioning env.font env.size env.fontColor (string_of_int (i+1)) in *)
        (* let (_,w,_)=boxes_interval (Array.of_list pnum) in *)
        (* let x=(fst page.pageFormat -. w)/.2. in *)
        (* let _=List.fold_left (fun x0 b->match b with *)
        (*                           (GlyphBox a)->( *)
        (*                             let (_,w,_)=box_interval b in *)
        (*                               page.pageContents<- (OutputCommon.Glyph { a with glyph_x=x0;glyph_y=20. }) *)
        (*                               :: page.pageContents; *)
        (*                               x0+.w *)
        (*                           ) *)
        (*                         | _ -> x0 *)
        (*                      ) x pnum *)
        (* in *)
          page.pageContents<-List.rev page.pageContents
    in
      for i=0 to Array.length pages-1 do draw_page i opt_pages.(i) done;
      List.iter (fun (p,link,dest)->try
                   let (p',x,y0,y1)=StrMap.find dest !destinations in
                     pages.(p).pageContents<-Link { link with dest_page=p'; dest_x=x; dest_y=y0+.(y1-.y0)*.phi }
                     ::pages.(p).pageContents
                 with
                     Not_found->()
                ) !crosslinks;
      M.output ~structure:(make_struct positions structure) pages file
end

