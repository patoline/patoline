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
open Typography
open Typography.Document
open Typography.Fonts.FTypes
open Typography.Box
open Typography.Layout
open Typography.Util


let centered parameters str tree _=
  newPar str ~environment:(fun x->{x with par_indent=[]}) Complete.normal parameters [
    bB (
      fun env->
        let spacing=1. in
        let r=0.3 in
        let x_height=
          let x=Fonts.loadGlyph env.font ({empty_glyph with glyph_index=Fonts.glyph_of_char env.font 'x'}) in
            (Fonts.glyph_y1 x)/.1000.
        in
        let orn=OutputCommon.translate 0. (env.size*.x_height/.2.-.r) (OutputCommon.Path ({OutputCommon.default with OutputCommon.fillColor=Some OutputCommon.black;OutputCommon.strokingColor=None }, [OutputCommon.circle r])) in
        let (orn_x0,_,orn_x1,_)=OutputCommon.bounding_box [orn] in
        let max_name=ref 0. in
        let max_w=ref 0. in
        let y=orn_x1-.orn_x0 in
        let rec toc env0 path tree=
          match tree with
              Paragraph p -> []
            | FigureDef f -> []
            | Node s when (List.length path) <= 1-> (
                let rec flat_children env1=function
                    []->[]
                  | (_,(FigureDef _))::s
                  | (_,(Paragraph _))::s->flat_children env1 s
                  | (k,(Node h as tr))::s->(
                      let env'=h.node_env env1 in
		      let chi1 = toc env' (k::path) tr in
		      chi1@flat_children (h.node_post_env env1 env') s
                    )
                in
                let chi=if List.mem_assoc "numbered" s.node_tags then flat_children env0 (IntMap.bindings s.children) else [] in
                let a,b=(try StrMap.find "_structure" (env0.counters) with _-> -1,[0]) in
                let count=drop 1 b in
                let in_toc=List.mem_assoc "intoc" s.node_tags in
                  if in_toc && count<>[] then (
                    let labl=String.concat "_" ("_"::List.map string_of_int path) in
                    let page=try
                               (1+page (UserMap.find (Label labl) (user_positions env0)))
                    with Not_found -> 0
                    in
                    let fenv env={ env with
                                     substitutions=
                        (fun glyphs->
                           List.fold_left (fun a b->Fonts.FTypes.apply b a)
                             (env.substitutions glyphs)
                             (Fonts.select_features env.font [ Fonts.Opentype.oldStyleFigures ]))
                                 }
                    in
                    let env'=fenv env0 in
                    let name= boxify_scoped env' s.displayname in
                    let pagenum=((boxify_scoped (fenv (envItalic true env0))
                                    [tT (Printf.sprintf "page %d" page)]))
                    in
                    let w_name=List.fold_left (fun w b->let (_,w',_)=box_interval b in w+.w') 0. name in
                    let w_page=List.fold_left (fun w b->let (_,w',_)=box_interval b in w+.w') 0. pagenum in
                    let cont=
                      (List.map (OutputCommon.translate (-.w_name-.spacing) 0.) (draw_boxes env name))@
                        orn::
                        (List.map (OutputCommon.translate (y+.spacing) 0.) (draw_boxes env pagenum))
                    in
                    max_w:=max !max_w (w_name+.w_page+.2.*.spacing);
                    max_name:=max !max_name (w_name+.spacing);
                    let (a,b,c,d)=OutputCommon.bounding_box cont in
                    Drawing {
                      drawing_min_width=env.normalMeasure;
                      drawing_nominal_width=env.normalMeasure;
                      drawing_max_width=env.normalMeasure;
		      drawing_width_fixed = true;
		      drawing_adjust_before = false;
                      drawing_y0=b;
                      drawing_y1=d;
                      drawing_break_badness=0.;
                      drawing_badness=(fun _->0.);
                      drawing_contents=(fun _->cont)
                    }::(glue 0. 0. 0.)::chi
                  )
                  else chi
              )
            | Node _->[]
        in
        let table=toc { env with counters=StrMap.add "_structure" (-1,[0]) env.counters }
          [] tree
        in
        let x0=if !max_name<env.normalMeasure*.1./.3. then env.normalMeasure/.2.
        else !max_name+.(env.normalMeasure-. !max_w)/.2.
        in
        List.map (function
                      Drawing d->
                        Drawing {d with drawing_contents=
                            (fun x->List.map (OutputCommon.translate x0 0.)
                               (d.drawing_contents x))
                                 }
                    | x->x) table
    )]


let these parameters str tree max_level=

  let params a b c d e f g line=
      parameters a b c d e f g line
  in
  newPar str ~environment:(fun x->{x with par_indent=[];lead=1.5*.x.lead}) Complete.normal params [
    bB (
      fun env->
        let margin=env.size*.phi in
        let rec toc env0 path tree=
          let level=List.length path in
          match tree with
              Paragraph p -> []
            | FigureDef f -> []
            | Node s when level <= max_level && List.mem_assoc "intoc" s.node_tags-> (
                let rec flat_children env1=function
                    []->[]
                  | (_,(FigureDef _))::s
                  | (_,(Paragraph _))::s->flat_children env1 s
                  | (k,(Node h as tr))::s->(
                      let env'=h.node_env env1 in
                      (toc env' (k::path) tr)@
                        flat_children (h.node_post_env env1 env') s
                    )
                in
                let chi=if List.mem_assoc "numbered" s.node_tags || path=[] then flat_children env0 (IntMap.bindings s.children) else [] in
                let a,b=(try StrMap.find "_structure" (env0.counters) with _-> -1,[0]) in
                let count=(List.rev (drop 1 b)) in
                let spacing=env.size/.phi in
                let in_toc=List.mem_assoc "intoc" s.node_tags in
                let numbered=List.mem_assoc "numbered" s.node_tags in
                if in_toc && count<>[] then (
                  let labl=String.concat "_" ("_"::List.map string_of_int path) in
                  let page=try
                             (1+page (UserMap.find (Label labl) (user_positions env0)))
                  with Not_found -> 0
                  in
                  let env'=add_features [Fonts.Opentype.oldStyleFigures] env in
                  let num=boxify_scoped { env' with fontColor=
                      if level=1 then OutputCommon.rgb 1. 0. 0. else OutputCommon.black }
                    [tT (String.concat "." (List.map (fun x->string_of_int (x+1)) count))] in
                  let name=boxify_scoped env' s.displayname in
                  let w=List.fold_left (fun w b->let (_,w',_)=box_interval b in w+.w') 0. num in
                  let w'=List.fold_left (fun w b->let (_,w',_)=box_interval b in w+.w') 0. name in
                  let cont=
                    (if numbered then List.map (OutputCommon.translate (-.w-.spacing) 0.)
                       (draw_boxes env num) else [])@
                      (List.map (OutputCommon.translate 0. 0.) (draw_boxes env name))@
                      List.map (OutputCommon.translate (w'+.spacing) 0.)
                      (draw_boxes env (boxify_scoped (envItalic true env') [tT (string_of_int page)]))
                  in
                  let (a,b,c,d)=OutputCommon.bounding_box cont in
                  User (BeginLink (labl))::
                    Drawing {
                      drawing_min_width=env.normalMeasure;
                      drawing_nominal_width=env.normalMeasure;
                      drawing_max_width=env.normalMeasure;
		      drawing_width_fixed = true;
		      drawing_adjust_before = false;
                      drawing_y0=b;
                      drawing_y1=d;
                      drawing_break_badness=0.;
                      drawing_badness=(fun _->0.);
                      drawing_contents=
                        (fun _->
                          List.map (OutputCommon.translate
                                      (margin+.spacing*.3.*.(float_of_int (level-1)))
                                       0.) cont)
                    }::User EndLink::(glue 0. 0. 0.)::chi
                )
                else chi
              )
            | Node _->[]
        in
        toc { env with counters=StrMap.add "_structure" (-1,[0]) env.counters }
          [] tree
    )]


let slides parameters str tree max_level=

  newPar str ~environment:(fun x->{x with par_indent=[]; lead=phi*.x.lead }) Complete.normal parameters [
    bB (
      fun env->
        let _,b0=try StrMap.find "_structure" env.counters with Not_found -> -1,[] in
        let rec prefix u v=match u,v with
          | [],_->true
          | hu::_,hv::_ when hu<>hv->false
          | _::su,_::sv->prefix su sv
          | _,[]->false
        in
        let rec toc env0 path tree=
          let level=List.length path in
          match tree with
              Paragraph p -> []
            | FigureDef f -> []
            | Node s when level <= max_level->(
                let rec flat_children env1=function
                    []->[]
                  | (_,(FigureDef _))::s
                  | (_,(Paragraph _))::s->flat_children env1 s
                  | (k,(Node h as tr))::s->(
                      let env'=h.node_env env1 in
                      (toc env' (k::path) tr)@
                        flat_children (h.node_post_env env1 env') s
                    )
                in
                let chi=if List.mem_assoc "numbered" s.node_tags || path=[] then flat_children env0 (IntMap.bindings s.children) else [] in
                let a,b=(try StrMap.find "_structure" (env0.counters) with _-> -1,[0]) in
                let count=(List.rev (drop 1 b)) in
                let spacing=env.size in
                let in_toc=List.mem_assoc "intoc" s.node_tags in
                let numbered=List.mem_assoc "numbered" s.node_tags in
                if in_toc && count<>[] then (
                  let labl=String.concat "_" ("_"::List.map string_of_int path) in

                  let env'=add_features [Fonts.Opentype.oldStyleFigures] env in

                  let c=0.8 in
                  let env_num=if b0=[] || prefix (List.rev b) (List.rev b0) && level=1 then
                      { env' with fontColor=OutputCommon.rgb 1. 0. 0. }
                    else
                      { env' with fontColor=OutputCommon.rgb c c c }
                  in
                  let env_name=if b0=[] || prefix (List.rev b) (List.rev b0) && level=1 then
                      { env' with fontColor=OutputCommon.rgb 0. 0. 0. }
                    else
                      { env' with fontColor=OutputCommon.rgb c c c }
                  in

                  let num=boxify_scoped env_num
                    [tT (String.concat "." (List.map (fun x->string_of_int (x+1)) count))] in

                  let name=boxify_scoped env_name s.displayname in


                  let w=List.fold_left (fun w b->let (_,w',_)=box_interval b in w+.w') 0. num in
                  let w0=2.*.env.size in
                  let cont=
                    (if numbered then List.map (OutputCommon.translate (w0-.w-.spacing) 0.)
                        (draw_boxes env_num num)
                     else [])@
                      (List.map (OutputCommon.translate w0 0.) (draw_boxes env' name))
                  in
                  let (a,b,c,d)=OutputCommon.bounding_box cont in
                  User (BeginLink (labl))::
                    Drawing {
                      drawing_min_width=env.normalMeasure;
                      drawing_nominal_width=env.normalMeasure;
                      drawing_max_width=env.normalMeasure;
		      drawing_width_fixed = true;
		      drawing_adjust_before = false;
                      drawing_y0=b;
                      drawing_y1=d;
                      drawing_break_badness=0.;
                      drawing_badness=(fun _->0.);
                      drawing_contents=
                        (fun _->
                           List.map (OutputCommon.translate
                                       (spacing*.3.*.(float_of_int (level-1)))
                                       0.) cont)
                    }::User EndLink::(glue 0. 0. 0.)::chi
                )
                else chi
              )
            | Node _->[]
        in
        toc { env with counters=StrMap.add "_structure" (-1,[0]) env.counters }
          [] tree
    )]
