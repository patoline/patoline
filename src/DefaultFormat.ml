open Typography.Document
open Typography.Parameters
open Typography.Fonts.FTypes
open Typography.Util
open Typography.Fonts
open Typography.Constants
open Typography.Binary

let _=Random.self_init ()

module Format=functor (D:Typography.Document.DocumentStructure)->(
  struct
    type user=Typography.Document.user
    let lmroman =
      [ Regular,(
        Lazy.lazy_from_fun (
          fun () ->simpleFamilyMember (loadFont (findFont "LatinModernRoman/lmroman10-regular.otf"))),
        Lazy.lazy_from_fun (
          fun ()->simpleFamilyMember (loadFont (findFont "LatinModernRoman/lmroman10-italic.otf"))));
      Bold, (
        Lazy.lazy_from_fun (
          fun () ->simpleFamilyMember (loadFont (findFont "LatinModernRoman/lmroman10-bold.otf"))),
        Lazy.lazy_from_fun (
          fun ()->simpleFamilyMember (loadFont (findFont "LatinModernRoman/lmroman10-bolditalic.otf"))));

      Caps, (
        Lazy.lazy_from_fun (
          fun () ->simpleFamilyMember (loadFont (findFont "LatinModernRoman/lmromancaps10-regular.otf"))),
        Lazy.lazy_from_fun (
          fun ()->simpleFamilyMember (loadFont (findFont "LatinModernRoman/lmromancaps10-oblique.otf"))));
      Demi, (
        Lazy.lazy_from_fun (
          fun () ->simpleFamilyMember (loadFont (findFont "LatinModernRoman/lmromandemi10-regular.otf"))),
        Lazy.lazy_from_fun (
          fun () ->simpleFamilyMember (loadFont (findFont "LatinModernRoman/lmromandemi10-oblique.otf"))))
    ]

  let lmmono =
    [ Regular, (
        Lazy.lazy_from_fun (
          fun () ->simpleFamilyMember (loadFont (findFont "LatinModernMono/lmmonolt10-regular.otf"))),
        Lazy.lazy_from_fun (
          fun () ->simpleFamilyMember (loadFont (findFont "LatinModernMono/lmmonolt10-oblique.otf"))));
      Bold, (
        Lazy.lazy_from_fun (
          fun () ->simpleFamilyMember (loadFont (findFont "LatinModernMono/lmmonolt10-bold.otf"))),
        Lazy.lazy_from_fun (
          fun () ->simpleFamilyMember (loadFont (findFont "LatinModernMono/lmmonolt10-boldoblique.otf"))));
      Caps, (
        Lazy.lazy_from_fun (
          fun () ->simpleFamilyMember (loadFont (findFont "LatinModernMono/lmmonocaps10-regular.otf"))),
        Lazy.lazy_from_fun (
          fun () ->simpleFamilyMember (loadFont (findFont "LatinModernMono/lmmonocaps10-oblique.otf"))));
      Demi, (
        Lazy.lazy_from_fun (
          fun () ->simpleFamilyMember (loadFont (findFont "LatinModernMono/lmmonoltcond10-regular.otf"))),
        Lazy.lazy_from_fun (
          fun () ->simpleFamilyMember (loadFont (findFont "LatinModernMono/lmmonoltcond10-oblique.otf"))));
    ]

  let defaultEnv:user environment=
    let f,str,subst,pos=selectFont lmroman Regular false in
    let hyphenate=try
      let i=open_in (findHyph "en.hdict") in
      let inp=input_value i in
        close_in i;
        (fun str->
           let hyphenated=Typography.Hyphenate.hyphenate inp str in
           let pos=Array.make (List.length hyphenated-1) ("","") in
           let rec hyph l i cur=match l with
               []->()
             | h::s->(
                 pos.(i)<-(cur^"-", List.fold_left (^) "" l);
                 hyph s (i+1) (cur^h)
               )
           in
             match hyphenated with
                 []->[||]
               | h::s->(hyph s 0 h; pos));
    with
        File_not_found _-> (fun x->[||])
    in
    let fsize=4. in
    let feat= [ Opentype.standardLigatures ] in
    let loaded_feat=Typography.Fonts.select_features f [ Opentype.standardLigatures ] in
      {
        fontFamily=lmroman;
        fontItalic=false;
        fontAlternative=Regular;
        fontFeatures=feat;
        fontColor=Typography.OutputCommon.black;
        font=f;
        word_substitutions=str;
        substitutions=(fun glyphs -> List.fold_left apply (subst glyphs) loaded_feat);
        positioning=(fun x->pos (positioning f x));
        footnote_y=10.;
        size=fsize;
        lead=fsize*.5./.4.;
        normalMeasure=150.;
        normalLead=fsize*.5./.4.;
        normalLeftMargin=(fst a4-.150.)/.2.;
        par_indent = [Drawing { drawing_min_width= 4.0 *. phi;
                                drawing_max_width= 4.0 *. phi;
                                drawing_y0=0.;drawing_y1=0.;
                                drawing_nominal_width= 4.0 *. phi;
                                drawing_contents=(fun _->[]);
                                drawing_badness=fun _-> 0. }];
        stdGlue=[Glue { drawing_min_width= 2.*. fsize/.9.;
                        drawing_max_width= fsize/.2.;
                        drawing_y0=0.; drawing_y1=0.;
                        drawing_nominal_width= fsize/.3.;
                        drawing_contents=(fun _->[]);
                        drawing_badness=knuth_h_badness (fsize/.3.) }];
        hyphenate=hyphenate;

        counters=StrMap.singleton "structure" (-1,[0]);
        names=StrMap.empty;
        user_positions=TS.UMap.empty;
        fixable=false
      }

  let title str ?label ?displayname name =
    let t0',path=
      match top !str with
          Node n,path -> Node { n with
                             name=name;
                             node_tags=Structural::InTOC::n.node_tags;
                             displayname = match displayname with
                                 None->[T name]
                               | Some a->a },path
        | t,path->Node { name=name;
                         node_tags=[Structural;InTOC];
                         displayname=(match displayname with
                                          Some a->a
                                        | None->[T name]);
		         children=IntMap.singleton 1 t;
                         node_env=(fun x->x);
                         node_post_env=(fun x y->{ x with names=y.names; counters=y.counters;
                                                     user_positions=y.user_positions });
                         tree_paragraph=0 },path
    in
      str:=follow (t0',[]) (List.rev path)
  let author str =
    D.structure:=match !D.structure with
        (Node h0,h1)->(Node { h0 with node_tags=(Author str)::
                           (List.filter (function Author _->false | _->true) h0.node_tags)
                            }, h1)
      | (h0,h1)->newChildAfter (
          Node { empty with
                   node_tags=(Author str)::
              (List.filter (function Author _->false | _->true) empty.node_tags)
               }, h1) h0
  let institute str =
    D.structure:=match !D.structure with
        (Node h0,h1)->(Node { h0 with node_tags=(Institute str)::
                              (List.filter (function Institute _->false | _->true) h0.node_tags)
                               }, h1)
      | (h0,h1)->newChildAfter (Node { empty with
                                            node_tags=(Institute str)::
                                       (List.filter (function Institute _->false | _->true)
                                          empty.node_tags)
                                        }, h1) h0
  let table_of_contents=TableOfContents.centered
  let postprocess_tree=Sections.postprocess_tree

  let lang_OCaml s=[T s]

  let minipage env str=
    let env',fig_params,params,compl,pars,figures=flatten env D.fixable (fst str) in
    let (_,pages,user')=TS.typeset
      ~completeLine:compl
      ~figure_parameters:fig_params
      ~figures:figures
      ~parameters:params
      ~badness:(Typography.Badness.badness pars)
      pars
    in
      OutputDrawing.output pars figures
        env'
        pages


  let footnote l=
    [Env (fun env->
            let next=match try snd (StrMap.find "footnotes" env.counters) with Not_found -> [] with
                []->0
              | h::_->h
            in
              { env with counters=StrMap.add "footnotes" (-1,[next+1]) env.counters });
     BFix (fun env0->
             let env= { env0 with normalMeasure=150.; normalLeftMargin=(fst a4-.150.)/.2. } in
             let count=match try snd (StrMap.find "footnotes" env.counters) with Not_found -> [] with
                 []->0
               | h::_->h
             in
             let foot_num=ref (-1) in
             let page_footnotes=ref 1 in
               TS.UMap.iter (fun k a->
                               match k with
                                   Footnote (i,_) when i= count -> foot_num:=a.page
                                 | _->()
                            ) env.user_positions;
               (* Y a-t-il deja des footnotes sur cette page ? *)
               TS.UMap.iter (fun k a->
                               match k with
                                   Footnote (i,_) when a.page= !foot_num && i< count ->
                                     incr page_footnotes
                                 | _->()
                            ) env.user_positions;
               (* Insertion d'une footnote *)
               let str=ref (Node empty,[]) in
               let params a b c d e f=
                 let p=(parameters a b c d e f) in
                 let lead=env.normalLead *. (phi-.1.) in
                   { p with min_height_after=lead }
               in
                 newPar str ~environment:(fun x->x) C.normal params
                   (T (string_of_int !page_footnotes)::(B (fun env->env.stdGlue))::l);
                 let pages=minipage { env with
                                        normalLead=env.lead*.(phi-.1.);
                                        lead=env.lead*.(phi-.1.);
                                        size=env.size*.(phi-.1.) }
                   (top !str)
                 in
                   if Array.length pages>0 then
                     [User (Footnote (count, pages.(0)));
                      Drawing (drawing ~offset:(env.size/.2.)
                                 (draw_boxes (boxify_scoped { env with size=env.size/.phi }
                                                [T (string_of_int !page_footnotes)])
                                 ))
                     ]
                   else
                     []
          )]

  let env_stack=ref []
  module Env_itemize = struct

    let do_begin_env ()=
      D.structure:=newChildAfter (!D.structure) (Node empty);
      env_stack:=snd !D.structure :: !env_stack

    let item ()=
      D.structure:=newChildAfter (follow (top !D.structure) (List.rev (List.hd !env_stack))) (Node empty);
      []

    let addon = [ T "-"; B (fun env->[glue env.size env.size env.size])]


    let do_end_env ()=
      let params params0 a b c d e f=
        let p=(params0 a b c d e f) in
        let boxes=boxify_scoped a addon in
        let w=List.fold_left (fun w0 b->w0+.box_width 0. b) 0. boxes in
          { p with
              left_margin=p.left_margin +. w;
              measure=p.measure-.w
          }
      in
      let params1 params0 a b c d e f=
        let p=params0 a b c d e f in
          if f.lineStart>0 then (
            let boxes=boxify_scoped a addon in
            let w=List.fold_left (fun w0 b->w0+.box_width 0. b) 0. boxes in
              { p with
                  left_margin=p.left_margin+.w;
                  measure=p.measure-.w
              }
          )
          else p
      in
      let rec paragraphs t=
        match t with
            Node n -> Node { n with children=IntMap.map paragraphs n.children }
          | Paragraph p ->
              Paragraph { p with
                            par_env=(fun x->
                                       let boxes=boxify_scoped x addon in
                                       let w=List.fold_left (fun w0 b->w0+.box_width 0. b) 0. boxes in
                                       let env=(p.par_env x) in
                                         { env with
                                             normalMeasure=env.normalMeasure -. w;
                                             par_indent=[]
                                         });
                            par_parameters=params p.par_parameters }
          | _ -> t
      in
      let rec tirets=function
          Node n as t ->
            (try
	       let chi=IntMap.map paragraphs n.children in
               let k,a=IntMap.min_binding n.children in
                 Node { n with children=IntMap.add k
                     (match a with
                        | Paragraph p ->
                            Paragraph { p with
                                          par_contents=addon@p.par_contents;
                                          par_parameters=params1 p.par_parameters
                                      }
                        | t -> t) chi }
	     with Not_found -> t)
        | t->t
      in
      let avec_tirets = match fst (follow (top !D.structure) (List.rev (List.hd !env_stack))) with
          Node n->Node { n with children=IntMap.map tirets n.children }
        | x->x
      in
        D.structure:=up (avec_tirets, List.hd !env_stack);
        env_stack:=List.tl !env_stack
  end

  module Env_abstract = struct

    let do_begin_env ()=
      newChildAfter !D.structure (Node empty);
      env_stack:=snd !D.structure :: !env_stack


    let do_end_env () =
      D.structure :=
        (fst (change_env (follow (top !D.structure) (List.rev (List.hd !env_stack)))
                (fun x->{ x with
                            normalLeftMargin=(x.normalLeftMargin
                                              +.(x.normalMeasure-.120.)/.2.);
                            normalMeasure=120. })), List.hd !env_stack);
      env_stack:=List.tl !env_stack

  end

  module type Theorem=sig
    val refType:string
    val counter:string
    val counterLevel:int
    val display:string->user content list
  end
  module Make_theorem=functor (Th:Theorem)->struct

    let reference name=generalRef ~refType:Th.refType name

    let do_begin_env ()=
      D.structure:=newChildAfter !D.structure (Node empty);
      env_stack:=snd !D.structure :: !env_stack

    let do_end_env ()=
      let rec first_par=function
          Paragraph p->
            Paragraph { p with par_contents=
                Env (fun env->incr_counter ~level:Th.counterLevel env Th.counter)::
                  CFix (fun env->
                          let lvl,num=try (StrMap.find Th.counter env.counters) with
                              Not_found -> -1,[0]
                          in
                          let _,str_counter=try
                            StrMap.find "structure" env.counters
                          with Not_found -> -1,[0]
                          in
                          let sect_num=drop (max 1 (List.length str_counter - lvl+1))
                            str_counter
                          in
                            Th.display (String.concat "." (List.map (fun x->string_of_int (x+1)) ((List.rev sect_num)@num)))
                       )::
                  B (fun env->env.stdGlue)::
                  p.par_contents
                      }
        | Node n->
            let k0=try fst (IntMap.min_binding n.children) with Not_found->0 in
            let paragraph=IntMap.singleton k0
              (first_par (Paragraph
                            { par_contents=[]; par_env=(fun x->x);
                              par_post_env=
                                (fun env1 env2 -> { env1 with names=env2.names;
                                                      counters=env2.counters;
                                                      user_positions=env2.user_positions });
                              par_parameters=parameters; par_completeLine=C.normal
                            }))
            in
              Node { n with children=IntMap.fold (fun k a b->IntMap.add (k+1) a b)
                  n.children paragraph }
        | x -> x
      in
      let stru=match follow (top !D.structure) (List.rev (List.hd !env_stack)) with
          Node n,_->
            (try
               let a,b=IntMap.min_binding n.children in
                 Node { n with children = IntMap.add a (first_par b) n.children }
             with
                 Not_found->first_par (Node n))
        | x,_->first_par x
      in
	D.structure := up (stru,List.hd !env_stack);
        env_stack:=List.tl !env_stack
  end
end)
