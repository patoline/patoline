open Typography
open CamomileLibrary
open Typography.Document
open Typography.Util
open Typography.Box
open Typography.Line


let postprocess_tree tree=
  let with_title=match tree with
      Node n->
        let par=Paragraph {
          par_contents=n.displayname;
          par_env=resize_env 8.;
          par_post_env=(fun env1 env2 -> { env1 with names=env2.names; counters=env2.counters;
                                             user_positions=env2.user_positions });
          par_parameters=
            (fun a b c d e f g->
               { (center a b c d e f g) with
                   min_height_after=if g.lineEnd>=Array.length b.(g.paragraph) then
                     2.*.a.normalLead else 0.;
                   min_height_before=0. });
          par_completeLine=Complete.normal }
        in
          fst (up (newChildBefore (tree,[]) par))
    | _->tree
  in
  let rec sectionize depth=function
      Node n when List.mem_assoc "Structural" n.node_tags ->
        let section_name=
          if List.mem_assoc "Numbered" n.node_tags  then
            [C (fun env->
                  let a,b=try StrMap.find "_structure" env.counters with Not_found -> -1,[] in
                  let _,path'=try StrMap.find "_path" env.counters with Not_found -> -1,[] in
                  let path=drop 1 b in
                    B (fun _->[User (Structure path')])
                    ::T (String.concat "." (List.map (fun x->string_of_int (x+1)) (List.rev path)))
                    ::T " "
                    ::n.displayname
               )]
          else
            B (fun env->
                 let _,path=try StrMap.find "_path" env.counters with Not_found -> -1,[] in
                   [User (Structure path)])::
              n.displayname
        in
        let par=Paragraph {
          par_contents=section_name;
          par_env=(fun env->
                     let a,b=try StrMap.find "_structure" env.counters with Not_found -> -1,[] in
                     let path=drop 1 b in
                       { (envAlternative (Opentype.oldStyleFigures::env.fontFeatures) Caps env) with
                           size=(if List.length path <= 1 then sqrt phi else
                                   sqrt (sqrt phi))*.env.size;
                       });
          par_post_env=(fun env1 env2 -> { env1 with names=env2.names; counters=env2.counters;
                                             user_positions=env2.user_positions });
          par_parameters=
            (fun a b c d e f g->
               { (parameters a b c d e f g) with
                   min_page_before = 0;(* (if depth=0 && f.lineStart=0 then 1 else 0); *)
                   min_height_before=if g.lineStart=0 then a.normalLead else 0.;
                   min_height_after=if g.lineEnd>=Array.length b.(g.paragraph) then a.normalLead else 0.;
                   not_last_line=true });
          par_completeLine=Complete.normal }
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
