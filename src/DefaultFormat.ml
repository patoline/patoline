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
    [ Regular, (
        Lazy.lazy_from_fun (fun () ->
	                      loadFont (findFont "LatinModernRoman/lmroman10-regular.otf")),
        Lazy.lazy_from_fun (fun () ->
	                      loadFont (findFont "LatinModernRoman/lmroman10-italic.otf")));
      Bold, (
        Lazy.lazy_from_fun (fun () ->
	                      loadFont (findFont "LatinModernRoman/lmroman10-bold.otf")),
        Lazy.lazy_from_fun (fun () ->
	                      loadFont (findFont "LatinModernRoman/lmroman10-bolditalic.otf")));
      Caps, (
        Lazy.lazy_from_fun (fun () ->
	                      loadFont (findFont "LatinModernRoman/lmromancaps10-regular.otf")),
        Lazy.lazy_from_fun (fun () ->
	                      loadFont (findFont "LatinModernRoman/lmromancaps10-oblique.otf")));
      Demi, (
        Lazy.lazy_from_fun (fun () ->
	                      loadFont (findFont "LatinModernRoman/lmromandemi10-regular.otf")),
        Lazy.lazy_from_fun (fun () ->
	                      loadFont (findFont "LatinModernRoman/lmromandemi10-oblique.otf")));
    ]

  let lmmono =
    [ Regular, (
        Lazy.lazy_from_fun (fun () ->
	                      loadFont (findFont "LatinModernMono/lmmonolt10-regular.otf")),
        Lazy.lazy_from_fun (fun () ->
	                      loadFont (findFont "LatinModernMono/lmmonolt10-oblique.otf")));
      Bold, (
        Lazy.lazy_from_fun (fun () ->
	                      loadFont (findFont "LatinModernMono/lmmonolt10-bold.otf")),
        Lazy.lazy_from_fun (fun () ->
	                      loadFont (findFont "LatinModernMono/lmmonolt10-boldoblique.otf")));
      Caps, (
        Lazy.lazy_from_fun (fun () ->
	                      loadFont (findFont "LatinModernMono/lmmonocaps10-regular.otf")),
        Lazy.lazy_from_fun (fun () ->
	                      loadFont (findFont "LatinModernMono/lmmonocaps10-oblique.otf")));
      Demi, (
        Lazy.lazy_from_fun (fun () ->
	                      loadFont (findFont "LatinModernMono/lmmonoltcond10-regular.otf")),
        Lazy.lazy_from_fun (fun () ->
	                      loadFont (findFont "LatinModernMono/lmmonoltcond10-oblique.otf")));
    ]

  let defaultEnv:user environment=
    let f=selectFont lmroman Regular false in
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
      {
        fontFamily=lmroman;
        fontItalic=false;
        fontAlternative=Regular;
        fontFeatures= [ Opentype.standardLigatures ];
        fontColor=Typography.OutputCommon.black;
        font=f;
        substitutions=
          (fun glyphs -> List.fold_left apply glyphs (
             Typography.Fonts.select_features f [ Opentype.standardLigatures ]
           ));
        positioning=positioning f;
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
    let (t,path),str1=match !str with
        []->(Node empty,[]),[]
      | h::s->h,s
    in
    let t0'=
      match t with
          Node n -> Node { n with
                             name=name;
                             displayname = match displayname with
                                 None->[T name]
                               | Some a->a }
        | _->Node { name=name;
                    node_tags=[];
                    displayname=(match displayname with
                                     Some a->a
                                   | None->[T name]);
		    children=IntMap.singleton 1 t;
                    node_env=(fun x->x);
                    node_post_env=(fun x y->{ x with names=y.names; counters=y.counters;
                                                user_positions=y.user_positions });
                    tree_paragraph=0 }
    in
      str:=(t0',path)::str1
  let author str =
    D.structure:=match !D.structure with
        (Node h0,h1)::s->(Node { h0 with node_tags=(Author str)::
                              (List.filter (function Author _->false | _->true) h0.node_tags)
                               }, h1)::s
      | (h0,h1)::s->newChildAfter (
          Node { empty with
                   node_tags=(Author str)::
              (List.filter (function Author _->false | _->true) empty.node_tags)
               }, h1) h0::s
      | _->
          [Node { empty with
                    node_tags=(Author str)::(List.filter (function Author _->false | _->true)
                                               empty.node_tags)
                }, []]
  let institute str =
    D.structure:=match !D.structure with
        (Node h0,h1)::s->(Node { h0 with node_tags=(Institute str)::
                              (List.filter (function Institute _->false | _->true) h0.node_tags)
                               }, h1)::s
      | (h0,h1)::s->newChildAfter (Node { empty with
                                            node_tags=(Institute str)::
                                       (List.filter (function Institute _->false | _->true)
                                          empty.node_tags)
                                        }, h1) h0::s
      | _->
          [Node { empty with
                    node_tags=(Author str)::
               (List.filter (function Author _->false | _->true) empty.node_tags)
                }, []]

  let table_of_contents=TableOfContents.centered
  let postprocess_tree=Sections.postprocess_tree

  let lang_OCaml s=[T s]

  let minipage (env) str=
    let env',fig_params,params,compl,pars,figures=flatten env (fst str) in
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
               let str=ref [Node empty,[]] in
               let params a b c d e f=
                 let p=(parameters a b c d e f) in
                 let lead=env.normalLead *. (phi-.1.) in
                   { p with min_height_after=lead }
               in
                 newPar str ~environment:(fun x->x) C.normal params
                   (T (string_of_int !page_footnotes)::(B (fun env->env.stdGlue))::l);
                 match !str with
                     []->assert false
                   | h::s->(
                       str:=s;
                       let pages=minipage { env with
                                              normalLead=env.lead*.(phi-.1.);
                                              lead=env.lead*.(phi-.1.);
                                              size=env.size*.(phi-.1.) }
                         (top h)
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
                     )
          )]


  module Env_itemize = struct

    let do_begin_itemize ()=
      D.structure := (Node empty, []):: !D.structure

    let item ()=
      let str0,str1=match !D.structure with []->(Node empty,[]),[] | h::s->h,s in
        D.structure:=newChildAfter (top str0) (Node empty)::str1;
        []

    let addon = [ T "–"; B (fun env->[glue env.size env.size env.size])]


    let do_end_itemize ()=
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
        match !D.structure with
            h0::h1::s ->
              let avec_tirets = match fst (top h0) with
                  Node n->Node { n with children=IntMap.map tirets n.children }
                | x->x
              in
	        D.structure := newChildAfter h1 (paragraphs avec_tirets)::s
          | _->assert false
  end

  module Env_abstract = struct

    let do_begin_abstract ()=
      D.structure := (Node empty, []):: !D.structure

    let do_end_abstract () =
      match !D.structure with
          h0::h1::s ->
	    D.structure :=
              up (newChildAfter h1
                    (fst (change_env h0
                            (fun x->{ x with
                                        normalLeftMargin=(x.normalLeftMargin
                                                          +.(x.normalMeasure-.120.)/.2.);
                                        normalMeasure=120. })))) :: s
        |_ -> assert false

  end

  let theoremRef name=generalRef ~refType:"theorem" "th0"
  module Env_theorem=struct
    let do_begin_theorem ()=
      D.structure := (Node empty, []):: !D.structure

    let do_end_theorem ()=
      match !D.structure with
          h0::h1::s ->
            let rec first_par=function
                Paragraph p->
                  Paragraph { p with par_contents=
                      Env (fun env->incr_counter ~level:1 env "theorem")::
                        CFix (fun env->
                                let lvl,num=(StrMap.find "theorem" env.counters) in
                                let _,str_counter=StrMap.find "structure" env.counters in
                                let sect_num=drop (List.length str_counter - lvl) str_counter in
                                  alternative Bold [T "Theorem"; B (fun env->env.stdGlue);T (String.concat "." (List.map (fun x->string_of_int (x+1)) (sect_num@num)))]
                             )::
                        B (fun env->env.stdGlue)::
                        p.par_contents
                            }
              | Node n->
                  let k0,_=IntMap.min_binding n.children in
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
            let stru=match fst h0 with
                Node n->
                  let a,b=IntMap.min_binding n.children in
                    Node { n with children = IntMap.add a (first_par b) n.children }
              | x->first_par x
            in
	      D.structure := up (newChildAfter h1 stru) :: s
        |_ -> assert false

    module Env_Proof=struct
    end
  end
end)
