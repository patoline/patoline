open Typography
open Document
open Fonts.FTypes
open Box
open Line
open Util


let centered str tree max_level=
  newPar str ~environment:(fun x->{x with par_indent=[]}) Complete.normal parameters [
    BFix (
      fun env->
        let x0=75. in
        let spacing=1. in
        let r=0.3 in
        let x_height=
          let x=Fonts.loadGlyph env.font ({empty_glyph with glyph_index=Fonts.glyph_of_char env.font 'x'}) in
            (Fonts.glyph_y1 x)/.1000.
        in
        let orn=OutputCommon.translate x0 (env.size*.x_height/.2.-.r) (OutputCommon.Path ({OutputCommon.default with OutputCommon.fillColor=Some OutputCommon.black;OutputCommon.strokingColor=None }, [OutputCommon.circle r])) in
        let (orn_x0,_,orn_x1,_)=OutputCommon.bounding_box [orn] in
        let y=orn_x1-.orn_x0 in
        let rec toc env0 level tree=
          match tree with
              Paragraph p -> []
            | FigureDef f -> []
            | Free _ -> []
            | Node s when level <= max_level-> (
                let rec flat_children env1=function
                    []->[]
                  | (_,(FigureDef _))::s
                  | (_,(Free _))::s
                  | (_,(Paragraph _))::s->flat_children env1 s
                  | (_,(Node h as tr))::s->(
                      let env'=h.node_env env1 in
                        (toc env' (level+1) tr)@
                          flat_children (h.node_post_env env1 env') s
                    )
                in
                let chi=flat_children env0 (IntMap.bindings s.children) in
                let a,b=(try StrMap.find "structure" (env0.counters) with _-> -1,[]) in
                let count=drop 1 b in
                let in_toc=List.mem_assoc "InTOC" s.node_tags in
                  if in_toc && count<>[] then (
                    try
                      let page=(1+(TS.UMap.find (Structure count) env0.user_positions).page) in
                      let fenv env={ env with
                                       substitutions=
                          (fun glyphs->
                             List.fold_left (fun a b->Fonts.FTypes.apply b a)
                               (env.substitutions glyphs)
                               (Fonts.select_features env.font [ Opentype.oldStyleFigures ]))
                                   }
                      in
                      let env'=fenv env0 in
                      let name= boxify_scoped env' s.displayname in
                      let w=List.fold_left (fun w b->let (_,w',_)=box_interval b in w+.w') 0. name in
                      let cont=
                        (List.map (OutputCommon.translate (x0-.w-.spacing) 0.) (draw_boxes name))@
                          orn::
                          List.map (OutputCommon.translate (x0+.y+.spacing) 0.)
                          (draw_boxes (boxify_scoped (fenv (envItalic true env0)) [T (Printf.sprintf "page %d" page)]))
                      in
                      let (a,b,c,d)=OutputCommon.bounding_box cont in
                        Drawing {
                          drawing_min_width=150.;
                          drawing_nominal_width=150.;
                          drawing_max_width=150.;
                          drawing_y0=b;
                          drawing_y1=d;
                          drawing_badness=(fun _->0.);
                          drawing_contents=(fun _->cont)
                        }::(glue 0. 0. 0.)::chi
                    with
                        Not_found -> chi
                  )
                  else chi
              )
            | Node _->[]
        in
          toc { env with counters=StrMap.add "structure" (-1,[0]) env.counters }
            0 (fst (top !str))
    )]
