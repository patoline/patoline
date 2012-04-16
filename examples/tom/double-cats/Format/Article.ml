


open Typography
open Typography.Fonts
open Typography.Fonts.FTypes
open Typography.Constants
open Typography.Document
open Util
open Binary
open CamomileLibrary
module CM = CamomileLibraryDefault.Camomile.CaseMap.Make(CamomileLibrary.UTF8)

module Format=functor (D:DocumentStructure)->struct
  type user=Document.user
  module Default=DefaultFormat.Format(D)
  let famille=Default.lmroman
    (* [ Regular, ( *)
    (*     Lazy.lazy_from_fun ( *)
    (*       fun () ->simpleFamilyMember (Fonts.loadFont (findFont "BemboStd.otf"))), *)
    (*     Lazy.lazy_from_fun ( *)
    (*       fun () ->simpleFamilyMember (Fonts.loadFont (findFont "BemboStd-Italic.otf")))); *)
    (*   Bold, ( *)
    (*     Lazy.lazy_from_fun ( *)
    (*       fun () ->simpleFamilyMember (Fonts.loadFont (findFont "BemboStd-Bold.otf"))), *)
    (*     Lazy.lazy_from_fun ( *)
    (*       fun () ->simpleFamilyMember (Fonts.loadFont (findFont "BemboStd-BoldItalic.otf")))); *)
    (*   Caps, ( *)
    (*     Lazy.lazy_from_fun ( *)
    (*       fun () -> *)
    (*         let f=Fonts.loadFont (findFont "BemboStd.otf") in *)
    (*         let subst=Fonts.select_features f [Opentype.smallCapitals] in *)
    (*           (f, *)
    (*            (fun x->CM.uppercase x), *)
    (*            (fun glyphs -> List.fold_left apply glyphs subst), *)
    (*            (fun x->x)) *)
    (*     ), *)
    (*     Lazy.lazy_from_fun ( *)
    (*       fun () -> *)
    (*         let f=Fonts.loadFont (findFont "BemboStd-Italic.otf") in *)
    (*         let subst=Fonts.select_features f [Opentype.smallCapitals] in *)
    (*           (f, *)
    (*            (fun x->CM.uppercase x), *)
    (*            (fun glyphs -> List.fold_left apply glyphs subst), *)
    (*            (fun x->x)) *)
    (*     )) *)
    (* ] *)

  let replace_utf8 x y z=
    Str.global_replace x
      (UTF8.init 1 (fun _->UChar.chr y)) z

  let defaultEnv=
    { (envFamily famille Default.defaultEnv) with
        word_substitutions=
        (fun x->List.fold_left (fun y f->f y) x
           [
             replace_utf8 (Str.regexp_string "``") 8220;
             replace_utf8 (Str.regexp_string "''") 8221
           ]
        )
    }
  let title=Default.title
  let author=Default.author
  let institute=Default.institute

let postprocess_tree tree=
  let with_title=match tree with
      Node n->
        let par=Paragraph {
          par_contents=n.displayname;
          par_env=resize_env 8.;
          par_post_env=(fun env1 env2 -> { env1 with names=env2.names; counters=env2.counters;
                                             user_positions=env2.user_positions });
          par_parameters=
            (fun a b c d e f->
               { (center a b c d e f) with
                   min_height_after=2.*.a.normalLead;
                   min_height_before=2.*.a.normalLead });
          par_completeLine=C.normal }
        in
          fst (up (newChildBefore (tree,[]) par))
    | _->tree
  in
  let rec sectionize depth=function
      Node n when List.mem Structural n.node_tags ->
        let section_name=
          if List.mem Numbered n.node_tags  then
            [C (fun env->
                  let a,b=try StrMap.find "structure" env.counters with Not_found -> -1,[] in
                  let _,path'=try StrMap.find "path" env.counters with Not_found -> -1,[] in
                  let path=drop 1 b in
                    B (fun _->[User (Structure path')])
                    ::T (String.concat "." (List.map (fun x->string_of_int (x+1)) (List.rev path)))
                    ::(B (fun env->env.stdGlue))
                    ::n.displayname
               )]
          else
            B (fun env->
                 let _,path=try StrMap.find "path" env.counters with Not_found -> -1,[] in
                   [User (Structure path)])::
              n.displayname
        in
        let par=Paragraph {
          par_contents=section_name;
          par_env=(fun env->
                     let a,b=try StrMap.find "structure" env.counters with Not_found -> -1,[] in
                     let path=drop 1 b in
                       { (envAlternative (Opentype.oldStyleFigures::env.fontFeatures) Caps env) with
                           size=(if List.length path <= 1 then sqrt phi else
                                   sqrt (sqrt phi))*.env.size;
                       });
          par_post_env=(fun env1 env2 -> { env1 with names=env2.names; counters=env2.counters;
                                             user_positions=env2.user_positions });
          par_parameters=
            (fun a b c d e f->
              { (parameters a b c d e f) with
                   min_page_before= 0;
                   min_page_after= 0;
                   (* if depth=0 && f.lineStart=0 then 1 else 0; *)
                   min_height_after=2.*.a.normalLead;
                   min_height_before=2.*.a.normalLead });
          par_completeLine=C.normal }
        in
          fst (up (newChildBefore (
                     Node { n with children=IntMap.map (sectionize (depth+1)) n.children }, []) par
                  ))
    | a->a
  in
  let with_chapters=match with_title with
      Node n->Node { n with children=IntMap.map (sectionize 0) n.children }
    | _->with_title
  in
    with_chapters

  module Env_definition=Default.Make_theorem
    (struct
       let refType="definition"
       let counter="definition"
       let counterLevel=3
       let display num=alternative Bold [T (Printf.sprintf "Définition %s" num);
                                         B (fun env->env.stdGlue)]
     end)

  open Util
  open Binary

  let table_of_contents str tree max_level=

    newPar str ~environment:(fun x->{x with par_indent=[]; lead=phi*.x.lead }) C.normal parameters [
      BFix (
        fun env->
          let rec toc env0 path tree=
            let level=List.length path in
            match tree with
                Paragraph p -> []
              | FigureDef f -> []
              | Node s when level <= max_level && List.mem InTOC s.node_tags-> (
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
                  let chi=flat_children env0 (Binary.IntMap.bindings s.children) in
                  let a,b=(try Binary.StrMap.find "structure" (env0.counters) with _-> -1,[]) in
                  let count=Binary.drop 1 b in
                  let spacing=env.size in
                  let in_toc=List.mem InTOC s.node_tags in
                  let numbered=List.mem Numbered s.node_tags in
                    if in_toc && count<>[] then (
                      try
                        let page=(1+(TS.UMap.find (Structure path) env0.user_positions).Util.page) in
                        let env'=add_features [Opentype.oldStyleFigures] env in
                        let num=boxify_scoped { env' with fontColor=
                            if level=1 then OutputCommon.rgb 255. 0. 0. else OutputCommon.black }
                          [T (String.concat "." (List.map (fun x->string_of_int (x+1)) (List.rev count)))] in
                        let name= boxify_scoped env' s.displayname in
                        let w=List.fold_left (fun w b->let (_,w',_)=box_interval b in w+.w') 0. num in
                        let w'=List.fold_left (fun w b->let (_,w',_)=box_interval b in w+.w') 0. name in
                        let cont=
                          (if numbered then List.map (OutputCommon.translate (-.w-.spacing) 0.)
                             (draw_boxes num) else [])@
                            (List.map (OutputCommon.translate 0. 0.) (draw_boxes name))@
                            List.map (OutputCommon.translate (w'+.spacing) 0.)
                            (draw_boxes (boxify_scoped (envItalic true env') [T (string_of_int page)]))
                        in
                        let (a,b,c,d)=OutputCommon.bounding_box cont in
                          Drawing {
                            drawing_min_width=env.normalMeasure;
                            drawing_nominal_width=env.normalMeasure;
                            drawing_max_width=env.normalMeasure;
                            drawing_y0=b;
                            drawing_y1=d;
                            drawing_badness=(fun _->0.);
                            drawing_contents=
                              (fun _->
                                 List.map (OutputCommon.translate
                                             (spacing*.3.*.(float_of_int (level-1)))
                                             0.) cont)
                          }::(glue 0. 0. 0.)::chi
                      with
                          Not_found -> chi
                    )
                    else chi
                )
              | Node _->[]
          in
            toc { env with counters=StrMap.add "structure" (-1,[0]) env.counters }
              [] (fst (top (List.hd !str)))
      )]

  let utf8Char x=[T (UTF8.init 1 (fun _->UChar.chr x))]
  let glyph x=
    B (fun env->
         let code={glyph_utf8=""; glyph_index=x } in
           [GlyphBox { (Util.glyphCache env.font code) with
                         OutputCommon.glyph_color=env.fontColor;
                         OutputCommon.glyph_size=env.size
                     }]
      )
  let q _=utf8Char 8220
  let qq _=utf8Char 8221

end
