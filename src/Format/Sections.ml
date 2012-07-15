open Typography
open CamomileLibrary
open Typography.Document
open Typography.Util
open Typography.Box
open Typography.Line


let postprocess_tree tree=

  let with_institute=match tree with
      Node n->(try
        let cont=[tT (List.assoc "Institute" n.node_tags)] in
        let par=Paragraph {
          par_contents=cont;
          par_env=(fun env->env);
          par_post_env=(fun env1 env2 -> { env1 with names=names env2; counters=env2.counters;
                                             user_positions=user_positions env2 });
          par_parameters=
            (fun a b c d e f g->
               { (center a b c d e f g) with
                   min_height_after=if g.lineEnd>=Array.length b.(g.paragraph) then
                     2.*.a.normalLead else 0.;
                   min_height_before=if g.lineEnd>=Array.length b.(g.paragraph) then
                     2.*.a.normalLead else 0.
               });
          par_badness=(badness);
          par_completeLine=Complete.normal }
        in
          fst (up (newChildBefore (tree,[]) par))
      with
          Not_found->tree)
    | _->tree
  in

  let with_author=match with_institute with
      Node n->(try
        let cont=[tT (List.assoc "Author" n.node_tags)] in
        let par=Paragraph {
          par_contents=cont;
          par_env=(fun env->env);
          par_post_env=(fun env1 env2 -> { env1 with names=names env2; counters=env2.counters;
                                             user_positions=user_positions env2 });
          par_parameters=
            (fun a b c d e f g->
               { (center a b c d e f g) with
                   min_height_after=if g.lineEnd>=Array.length b.(g.paragraph) then
                     2.*.a.normalLead else 0.;
                   min_height_before=if g.lineEnd>=Array.length b.(g.paragraph) then
                     2.*.a.normalLead else 0.
               });
          par_badness=(badness);
          par_completeLine=Complete.normal }
        in
          fst (up (newChildBefore (with_institute,[]) par))
      with
          Not_found->with_institute)
    | _->with_institute
  in

  let with_title=match with_author with
      Node n->
        let par=Paragraph {
          par_contents=n.displayname;
          par_env=resize_env 8.;
          par_post_env=(fun env1 env2 -> { env1 with names=names env2; counters=env2.counters;
                                             user_positions=user_positions env2 });
          par_parameters=
            (fun a b c d e f g->
               { (center a b c d e f g) with
                   min_height_after=if g.lineEnd>=Array.length b.(g.paragraph) then
                     a.normalLead else 0.;
                   min_height_before=0. });
          par_badness=(badness);
          par_completeLine=Complete.normal }
        in
          fst (up (newChildBefore (with_author,[]) par))
    | _->with_author
  in

  let rec sectionize path=function
      Node n when List.mem_assoc "Structural" n.node_tags ->
        let section_name=
          if List.mem_assoc "Numbered" n.node_tags  then
            [C (fun env->
                  let a,b=try StrMap.find "_structure" env.counters with Not_found -> -1,[0] in
                  bB (fun _->[User (Structure path)])
                  ::tT (String.concat "." (List.map (fun x->string_of_int (x+1)) (List.rev (drop 1 b))))
                  ::tT " "
                  ::n.displayname
               )]
          else
            [C(fun env->
                 bB (fun env->[User (Structure path)])::
                   n.displayname)]
        in
        let par=Paragraph {
          par_contents=section_name;
          par_env=(fun env->
                     let a,b=try StrMap.find "_structure" env.counters with Not_found -> -1,[0] in
                     { (envAlternative (Opentype.oldStyleFigures::env.fontFeatures) Caps env) with
                         size=(if List.length b <= 2 then sqrt phi else
                                 sqrt (sqrt phi))*.env.size;
                     });
          par_post_env=(fun env1 env2 -> { env1 with names=names env2; counters=env2.counters;
                                             user_positions=user_positions env2 });
          par_parameters=
            (fun a b c d e f g->
               { (parameters a b c d e f g) with
                   min_page_before = 0;
                   min_height_before=if g.lineStart=0 then a.normalLead else 0.;
                   min_height_after=if g.lineEnd>=Array.length b.(g.paragraph) then a.normalLead else 0.;
                   not_last_line=true });
          par_badness=(badness);
          par_completeLine=Complete.normal }
        in
          fst (up (newChildBefore (
                     Node { n with children=IntMap.mapi (fun k a->sectionize (k::path) a)
                         n.children }, []) par
                  ))
    | a->a
  in
  let with_chapters=match with_title with
      Node n->Node { n with children=IntMap.map (sectionize []) n.children }
    | _->with_title
  in
    with_chapters
