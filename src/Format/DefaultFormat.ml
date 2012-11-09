open Typography
open Typography.Document
open Typography.Complete
open Typography.Fonts.FTypes
open Typography.Util
open Typography.Fonts
open Typography.Box
open Typography.Line
open CamomileLibrary

let _=Random.self_init ()

module Euler = Euler
module Numerals = Numerals

let replace_utf8 x y z=if String.length x>0 then (
  let buf=Buffer.create (String.length x) in
  let repl=UTF8.init 1 (fun _->UChar.chr y) in
  let rec add_it i=
    if not (UTF8.out_of_range z i) then (
      try
        let rec comp j=
          if UTF8.out_of_range x j then j else
            if UTF8.out_of_range z (i+j) then raise Not_found else
              if UTF8.look z (i+j) <> UTF8.look x j then raise Not_found else
                comp (UTF8.next x j)
        in
        let j=comp 0 in
        Buffer.add_string buf repl;
        add_it (i+j)
      with
          Not_found->(
            Buffer.add_string buf (String.sub z i (UTF8.next z i-i));
            add_it (UTF8.next z i)
          )
    )
  in
  add_it 0;
  Buffer.contents buf
) else z

let word_subst=
  (fun x->List.fold_left (fun y f->f y) x
    [
      replace_utf8 ("``") 8220;
      replace_utf8 ("''") 8221
    ]
  )

let alegreya=
  [ Regular,
    (Lazy.lazy_from_fun
       (fun ()->
         (Fonts.loadFont (findFont "Alegreya/Alegreya-Regular.otf")),
         word_subst,
          (fun x->List.fold_left (fun a f->f a) x
             [make_ligature [168;175] {glyph_utf8="fi";glyph_index=245};
              make_ligature [168;181] {glyph_utf8="fl";glyph_index=246};
              make_ligature [168;177] {glyph_utf8="fj";glyph_index=383};
              make_ligature [175;177] {glyph_utf8="ij";glyph_index=176};
             ]),
          (fun x->x)),
     Lazy.lazy_from_fun
       (fun ()->
          (Fonts.loadFont (findFont "Alegreya/Alegreya-Italic.otf")),
         word_subst,
          (fun x->List.fold_left (fun a f->f a) x
             [make_ligature [162;170] {glyph_utf8="fi";glyph_index=477};
              make_ligature [162;175] {glyph_utf8="fl";glyph_index=478};
              make_ligature [162;171] {glyph_utf8="fj";glyph_index=482};
              make_ligature [170;171] {glyph_utf8="ij";glyph_index=476};
             ]),
          (fun x->x)));
    Bold,
    (Lazy.lazy_from_fun
       (fun ()->
          (Fonts.loadFont (findFont "Alegreya/Alegreya-Bold.otf")),
         word_subst,
          (fun x->List.fold_left (fun a f->f a) x
             [make_ligature [168;175] {glyph_utf8="fi";glyph_index=245};
              make_ligature [168;181] {glyph_utf8="fl";glyph_index=246};
              make_ligature [168;177] {glyph_utf8="fj";glyph_index=383};
              make_ligature [175;177] {glyph_utf8="ij";glyph_index=176};
             ]),
          (fun x->x)),
     Lazy.lazy_from_fun
       (fun ()->
          (Fonts.loadFont (findFont "Alegreya/Alegreya-BoldItalic.otf")),
         word_subst,
          (fun x->List.fold_left (fun a f->f a) x
             [make_ligature [162;170] {glyph_utf8="fi";glyph_index=477};
              make_ligature [162;175] {glyph_utf8="fl";glyph_index=478};
              make_ligature [162;171] {glyph_utf8="fj";glyph_index=482};
              make_ligature [170;171] {glyph_utf8="ij";glyph_index=476};
             ]),
          (fun x->x)));
    Caps,
    (simpleFamilyMember (fun ()->Fonts.loadFont (findFont "Alegreya/AlegreyaSC-Regular.otf")),
     simpleFamilyMember (fun ()->Fonts.loadFont (findFont "Alegreya/AlegreyaSC-Italic.otf")));
  ]

let texgyrecursor=
  [ Regular,
    (Lazy.lazy_from_fun
       (fun ()->
         (Fonts.loadFont (findFont "TexGyreCursor/texgyrecursor-regular.otf")),
          (fun x->x),
          (fun x->x),
          (fun x->x)),
     Lazy.lazy_from_fun
       (fun ()->
          (Fonts.loadFont (findFont "TexGyreCursor/texgyrecursor-italic.otf")),
          (fun x->x),
          (fun x->x),
          (fun x->x)));
    Bold,
    (Lazy.lazy_from_fun
       (fun ()->
          (Fonts.loadFont (findFont "TexGyreCursor/texgyrecursor-bold.otf")),
          (fun x->x),
          (fun x->x),
          (fun x->x)),
     Lazy.lazy_from_fun
       (fun ()->
          (Fonts.loadFont (findFont "TexGyreCursor/texgyrecursor-bolditalic.otf")),
          (fun x->x),
          (fun x->x),
          (fun x->x)));

  ]

let bitstreamverasansmono=
  [ Regular,
    (Lazy.lazy_from_fun
       (fun ()->
         (Fonts.loadFont (findFont "BitstreamVeraSansMono/BitstreamVeraSansMono-Roman.otf")),
          (fun x->x),
          (fun x->x),
          (fun x->x)),
     Lazy.lazy_from_fun
       (fun ()->
          (Fonts.loadFont (findFont "BitstreamVeraSansMono/BitstreamVeraSansMono-Oblique.otf")),
          (fun x->x),
          (fun x->x),
          (fun x->x)));
    Bold,
    (Lazy.lazy_from_fun
       (fun ()->
          (Fonts.loadFont (findFont "BitstreamVeraSansMono/BitstreamVeraSansMono-Bold.otf")),
          (fun x->x),
          (fun x->x),
          (fun x->x)),
     Lazy.lazy_from_fun
       (fun ()->
          (Fonts.loadFont (findFont "BitstreamVeraSansMono/BitstreamVeraSansMono-BoldOb.otf")),
          (fun x->x),
          (fun x->x),
          (fun x->x)));

  ]

let all_fonts = [alegreya; texgyrecursor] (* trick to force same type *)

let font_size_ratio font1 font2 =
  let x_h f =
    let f,_,_,_ = Lazy.force (fst (List.assoc Regular f)) in
    let x=Fonts.loadGlyph f
      ({empty_glyph with glyph_index=Fonts.glyph_of_char f 'x'}) in
    Fonts.glyph_y1 x -.  Fonts.glyph_y0 x
  in
  x_h font1 /. x_h font2

module Format=functor (D:Document.DocumentStructure)->(
  struct

    let bold a=alternative Bold a

    let sc a=alternative Caps a

    let verbEnv x =
	{ (envFamily x.fontMonoFamily x)
	with size = x.size *. x.fontMonoRatio; normalMeasure=infinity; par_indent = [];
	     lead = x.lead *. x.fontMonoRatio}

    let verb p =
       [Scoped ((fun x ->
	 { (envFamily x.fontMonoFamily x) with size = x.size *. x.fontMonoRatio}), p)]

    let emph=toggleItalic
    let id x=x

    let sourcePosition(file,line,column,char) =
      [tT (Printf.sprintf "%s: %d,%d (%d)" file line column char)]

    let node l=
      Document.Node
        {Document.empty with
           Document.children=List.fold_left
            (fun m (l,_)->Util.IntMap.add (Util.IntMap.cardinal m) l m) Util.IntMap.empty l},
      []


    let parameters env paragraphs figures last_parameters last_figures last_users (last_line:line) (line:line)=
      let page_footnotes=ref 0 in
      let measure=IntMap.fold (fun i aa m->match aa with
          Break.Placed a->
            (if line.page=a.page &&
               line.height<=
               a.height -.figures.(i).drawing_y0
             && line.height>=
               a.height-. figures.(i).drawing_y1
             then
                env.normalMeasure -. figures.(i).drawing_nominal_width
             else m)
        | _->m
      ) last_figures env.normalMeasure
      in
      UserMap.iter (fun k a->
        match k with
            Footnote _ when a.page=line.page -> incr page_footnotes
          | _->()
      ) last_users;
      let footnote_h=fold_left_line paragraphs (fun fn box->match box with
          User (Footnote (_,x))->fn+.x.drawing_y1-.x.drawing_y0
        | _->fn) 0. line
      in
      { measure=measure;
        page_height=min  240. (if line.page_line <= 0 then 45.*.env.normalLead else last_parameters.page_height)
        -. (if footnote_h>0. && !page_footnotes=0 then (footnote_h+.env.footnote_y) else footnote_h);
        left_margin=env.normalLeftMargin;
        local_optimization=0;
        min_page_before=0;
        min_page_after=0;
        next_acceptable_height=(fun node params nextNode nextParams height->
          if node==nextNode then
            let min_height=node.height+.params.min_height_after in
            env.lead*.(ceil (min_height/.env.lead))
          else
            if node.page=nextNode.page then (
              let min_height=max (nextNode.height+.env.lead) (node.height +. max params.min_height_after nextParams.min_height_before) in
              let h0=min_height/.env.lead in
              let h=if h0-.floor h0 < 1e-10 then env.lead*.floor h0 else
                  env.lead*.ceil h0
              in
              h
            ) else (
              let min_height=nextNode.height+.max nextParams.min_height_before env.lead in
              env.lead*.(ceil (min_height/.env.lead))
            )
        );
        min_height_before=0.;
        min_height_after=0.;
        not_last_line=false;
        not_first_line=false;
        really_next_line=1;
        absolute=false
      }
    let center = do_center parameters
    let ragged_right = do_ragged_right parameters
    let ragged_left = do_ragged_left parameters


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
                         (fun a b c d e f g line->
                           { (center a b c d e f g line) with
                             min_height_after=if line.lineEnd>=Array.length b.(line.paragraph) then
                                 2.*.a.normalLead else 0.;
                             min_height_before=if g.lineEnd>=Array.length b.(g.paragraph) then
                                 2.*.a.normalLead else 0.
                           });
                       par_badness=(badness);
                       par_completeLine=Complete.normal;
                       par_states=IntSet.empty;
                       par_paragraph=(-1) }
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
                       par_env=(fun env->{env with par_indent=[]});
                       par_post_env=(fun env1 env2 -> { env1 with names=names env2; counters=env2.counters;
                         user_positions=user_positions env2 });
                       par_parameters=
                         (fun a b c d e f g line->
                           { (center a b c d e f g line) with
                             min_height_after=if line.lineEnd>=Array.length b.(line.paragraph) then
                                 2.*.a.normalLead else 0.;
                             min_height_before=if line.lineEnd>=Array.length b.(line.paragraph) then
                                 2.*.a.normalLead else 0.
                           });
                       par_badness=(badness);
                       par_completeLine=Complete.normal;
                       par_states=IntSet.empty;
                       par_paragraph=(-1) }
                     in
                     fst (up (newChildBefore (with_institute,[]) par))
            with
                Not_found->with_institute)
        | _->with_institute
      in

      let with_title=match tree with
          Node n->
            let par=Paragraph {
              par_contents=n.displayname;
              par_env=resize_env 3.;
              par_post_env=(fun env1 env2 -> { env1 with names=names env2; counters=env2.counters;
                user_positions=user_positions env2 });
              par_parameters=
                (fun a b c d e f g line->
                  { (center a b c d e f g line) with
                    min_height_after=if line.lineEnd>=Array.length b.(line.paragraph) then
                        a.normalLead else 0.;
                    min_height_before=0. });
              par_badness=(badness);
              par_completeLine=Complete.normal;
              par_states=IntSet.empty;
              par_paragraph=(-1)}
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
            { (envAlternative (Fonts.Opentype.oldStyleFigures::env.fontFeatures) Caps env) with
              size=(if List.length b <= 2 then sqrt phi else
                  sqrt (sqrt phi))*.env.size;
            });
          par_post_env=(fun env1 env2 -> { env1 with names=names env2; counters=env2.counters;
            user_positions=user_positions env2 });
          par_parameters=
            (fun a b c d e f g line->
              { (parameters a b c d e f g line) with
                min_height_before=if line.lineStart=0 then a.normalLead else 0.;
                min_height_after=if line.lineEnd>=Array.length b.(line.paragraph) then a.normalLead else 0.;
                not_last_line=true });
          par_badness=(badness);
          par_completeLine=Complete.normal;
          par_states=IntSet.empty;
          par_paragraph=(-1) }
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

    let paragraph ?(parameters=parameters) ?(par_env=(fun x->x)) cont=
      (Paragraph {par_contents=cont; par_env=par_env;
                  par_post_env=(fun env1 env2 -> { env1 with names=env2.names;
                                                     counters=env2.counters;
                                                     user_positions=env2.user_positions });
                  par_badness=(badness);
                  par_parameters=parameters; par_completeLine=Complete.normal;
                  par_states=IntSet.empty;
                  par_paragraph=(-1)}, [])

    module Env_Noindent=struct
      let do_begin_env ()=()
      let do_end_env ()=()
      let newPar str ~environment complete params contents=
        Document.newPar str ~environment:(fun x->{x with par_indent = []})
          complete params contents
    end

    let defaultEnv:environment=
      let f,str,subst,pos=selectFont alegreya Regular false in
      let hyphenate=
      	try
        let i=open_in_bin (findHyph "en.hdict") in
        let inp=input_value i in
          close_in i;
          (fun str->
             let hyphenated=Hyphenate.hyphenate inp str in
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
          File_not_found (f,p)->
	    (Printf.fprintf stderr "Warning : no hyphenation dictionary (%s not found). Path :\n" f;
                                  List.iter (Printf.fprintf stderr "%s\n") p;
                                  fun x->[||])
      in
      let fsize=3.8 in
      let feat= [ Opentype.standardLigatures ] in
      let loaded_feat=Fonts.select_features f [ Opentype.standardLigatures ] in
        {
          fontFamily=alegreya;
          fontMonoFamily=bitstreamverasansmono (*texgyrecursor*);
	  fontMonoRatio=font_size_ratio alegreya bitstreamverasansmono (*texgyrecursor*);
          fontItalic=false;
          fontAlternative=Regular;
          fontFeatures=feat;
          fontColor=OutputCommon.black;
          font=f;
          mathsEnvironment=Euler.default;
	  mathStyle=Document.Mathematical.Text;
          word_substitutions=
            (fun x->List.fold_left (fun y f->f y) x
               [
                 replace_utf8 ("``") 8220;
                 replace_utf8 ("''") 8221
               ]
            );
          substitutions=(fun glyphs -> List.fold_left (fun a b->apply b a) (subst glyphs) loaded_feat);
          positioning=(fun x->pos (positioning f x));
          footnote_y=10.;
          size=fsize;
          lead=13./.10.*.fsize;
          normalMeasure=150.;
          normalLead=13./.10.*.fsize;
          normalLeftMargin=(fst a4-.150.)/.2.;
          par_indent = [Drawing { drawing_min_width= 4.0 *. phi;
                                  drawing_max_width= 4.0 *. phi;
                                  drawing_y0=0.;drawing_y1=0.;
                                  drawing_nominal_width= 4.0 *. phi;
                                  drawing_contents=(fun _->[]);
                                  drawing_badness=fun _-> 0. }];
          hyphenate=hyphenate;
          counters=StrMap.empty;
          names=StrMap.empty;
          user_positions=UserMap.empty;
	  show_boxes=false;
        }


    let title str ?label ?(extra_tags=[]) displayname =
      let displayname=[C (fun _->env_accessed:=true;displayname)] in
      try
	let name = string_of_contents displayname in
	let t0',path=
          match top !str with
            Node n,path ->
	      if List.mem_assoc "MainTitle" n.node_tags then
		raise Exit;
	      Node { n with
                name=name;
                node_tags=("MainTitle","")::("Structural","")::("InTOC","")::extra_tags@n.node_tags;
                displayname = displayname},path
          | t,path->
	    Node { empty with
              name=name;
              node_tags=["Structural","";"InTOC",""];
              displayname=displayname;
	      children=IntMap.singleton 1 t;
              node_env=(fun x->x);
              node_post_env=(fun x y->{ x with names=y.names; counters=y.counters;
                user_positions=y.user_positions });
              node_states=IntSet.empty
            },path
	in
        str:=follow (t0',[]) (List.map fst (List.rev path)); true
      with
	Exit ->
	  newStruct D.structure displayname; false

    module TableOfContents=struct
      let do_begin_env ()=
        let max_depth=2 in
        TableOfContents.these center D.structure (fst (top !D.structure)) max_depth
      let do_end_env ()=()
    end


    let split_space is_letter is_special s =
      let gl env =
	let font,_,_,_=selectFont env.fontFamily Regular false in
	glyph_of_string env.substitutions env.positioning font env.size env.fontColor " "
      in
      let space = bB(fun env -> gl env) in
      let len = String.length s in
      let rec fn acc w i0 i =
	if i >= len then
	  List.rev (tT (String.sub s i0 (len - i0))::acc)
	else if s.[i] = ' ' then
	  let acc = if i <> i0 then
	      space::tT (String.sub s i0 (i - i0))::acc
	    else
	      space::acc
	  in
	  fn acc None (i+1) (i+1)
	else if is_special s.[i] then
	  let acc = if i <> i0 then
	    tT(String.sub s i 1)::tT (String.sub s i0 (i - i0))::acc
	    else
	      tT(String.sub s i 1)::acc
	  in
	  fn acc None (i+1) (i+1)
	else if w =None then
	  fn acc (Some (is_letter s.[i])) i0 (i+1)
	else if w = Some (is_letter s.[i]) then
	  fn acc  w i0 (i + 1)
	else
	  fn (tT (String.sub s i0 (i - i0))::acc) (Some (is_letter s.[i])) i (i+1)
      in fn [] None 0 0

    let is_letter_ml = fun c ->
      let c = Char.code c in
      (Char.code 'a' <= c && c <= Char.code 'z') ||
      (Char.code 'A' <= c && c <= Char.code 'Z') ||
      (Char.code '0' <= c && c <= Char.code '9') ||
      Char.code '_' = c || c = Char.code '\''


    let verb_counter filename =
      let get_line env =
	  try 
	    match StrMap.find filename env.counters
	    with a,[line] -> line
	    | _ -> raise Not_found
	  with Not_found -> 1
      in
      C (fun env ->
	let line = string_of_int (get_line env) in
	let miss = 4 - String.length line in
	let rec fn acc n = if n = 0 then acc else fn (tT " "::acc) (n - 1) in
	fn [tT line;tT " "] miss)::
      Env (fun env ->
	let line = get_line env in
	{env with counters = StrMap.add filename (-1,[line+1]) env.counters})::
	[]

    let lang_ML keywords specials s =
      let l = split_space is_letter_ml (fun c -> List.mem c specials) s in
      List.rev (List.fold_left (fun a x ->
	match x with
            T (s,_) as x when List.mem s keywords -> bold [x]@a
	| x -> x::a) [] l)

    let lang_default str = split_space (fun _ -> true) (fun _ -> false) str

    let lang_SML s=
      let specials = ['(';')';';'] in
      let keywords = ["fun";"as";"fn";"*";"(";")";",";";";"val";"and";"=>";"->";"type";"|";"=";
			  "case";"of";"datatype";"let";"rec";"end"] in
      lang_ML keywords specials s

    let lang_OCaml s=
      let specials = ['(';')';';'] in
      let keywords = ["fun";"as";"function";"(";")";"*";";";",";"val";"and";"=>";"->";"type";"|";"=";
		      "match";"with";"rec";"let";"begin";"end";"while";"for";"do";"done";
		      "struct"; "sig"; "module"; "functor"] in
      lang_ML keywords specials s

    let minipage env str=
      let env',fig_params,params,compl,bads,pars,par_trees,figures,figure_trees=flatten env D.fixable (fst str) in
      let (_,pages,fig',user')=TS.typeset
        ~completeLine:compl
        ~figure_parameters:fig_params
        ~figures:figures
        ~parameters:params
        ~badness:bads
        pars
      in
      (OutputDrawing.output pars figures
         env'
         pages)

    let minipage' env str=
      let env',fig_params,params,compl,bads,pars,par_trees,figures,figure_trees=flatten env D.fixable (fst str) in
      let (_,pages,fig',user')=TS.typeset
        ~completeLine:compl
        ~figure_parameters:fig_params
        ~figures:figures
        ~parameters:params
        ~badness:bads
        pars
      in
      (OutputDrawing.output pars figures
         env'
         pages,env')

    let figure ?(parameters=center) ?(name="") ?(caption=[]) ?(scale=1.) drawing=
      let drawing' env=
        let dr_=drawing env in
        let dr=
          if scale<>1. then
            match resize scale (Drawing dr_) with Drawing f->f | _->assert false
          else dr_
        in
        let lvl,num=try StrMap.find "figure" env.counters with Not_found -> -1,[] in
        let _,str_counter=try StrMap.find "_structure" env.counters with Not_found -> -1,[] in
        let sect_num=drop (List.length str_counter - max 0 lvl+1) str_counter in
	let caption =
	  minipage {env with normalLeftMargin=0.} (
	    paragraph ((
              [ tT "Figure"; tT " ";
                tT (String.concat "." (List.map (fun x->string_of_int (x+1)) (List.rev (num@sect_num)))) ]
              @(if caption=[] then [] else tT" "::tT"-"::tT" "::caption)
            )))
	in
        let caption= caption.(0) in
        let fig=if caption.drawing_nominal_width<=dr.drawing_nominal_width then
          drawing_blit dr
            ((dr.drawing_nominal_width-.caption.drawing_nominal_width)/.2.)
            (dr.drawing_y0-.2.*.caption.drawing_y1) caption
        else
          drawing_blit caption
            ((caption.drawing_nominal_width-.dr.drawing_nominal_width)/.2.)
            (2.*.caption.drawing_y1-.dr.drawing_y0) dr
        in
        { fig with drawing_y0=fig.drawing_y0-.env.lead }
      in
      figure ~name:name D.structure center drawing'



    type tableParams={ widths:environment->float array; h_spacing:float; v_spacing:float }

    let table params tab=
      [ bB (fun env->
             let widths0=params.widths env in
             let widths=Array.make (Array.length widths0) 0. in
             let heights=Array.make (Array.length tab) 0. in
             let tab_formatted=Array.mapi
               (fun i x->
                  Array.mapi (fun j y->
                                let minip=(minipage
                                             { env with normalMeasure=widths0.(j) } y).(0) in
                                  widths.(j)<-max widths.(j) (minip.drawing_max_width);
                                  heights.(i)<-max heights.(i) (minip.drawing_y1-.minip.drawing_y0);
                                  minip
                             ) x
               )
               tab
             in
             let contents=ref [] in
             let x=ref 0. in
             let y=ref 0. in
             let max_y=ref (-.infinity) in
             let min_y=ref infinity in
             let ymin=ref 0. in
             let ymax=ref 0. in
               for i=0 to Array.length tab_formatted-1 do
                 x:=0.;
                 ymin:=0.;
                 ymax:= -.infinity;
                 let conts=ref [] in
                   for j=0 to Array.length tab_formatted.(i)-1 do
                     let cont=tab_formatted.(i).(j) in
                       conts:=(List.map (OutputCommon.translate !x 0.)
                                 (cont.drawing_contents (widths.(j)))) @ (!conts);
                       ymin := min !ymin cont.drawing_y0;
                       ymax := max !ymax cont.drawing_y1;
                       x:= !x +. widths.(j) +. params.h_spacing
                   done;
                   contents:=(List.map (OutputCommon.translate 0. !y) !conts)@(!contents);
                   max_y:=max !max_y (!y+. !ymax);
                   min_y:=min !min_y (!y+. !ymin);
                   y:=(!y)-. !ymax +. !ymin;
               done;

               [Drawing {
                  drawing_min_width= !x;
                  drawing_max_width= !x;
                  drawing_nominal_width= !x;
                  drawing_y0= !min_y;
                  drawing_y1= !max_y;
                  drawing_badness=(fun _->0.);
                  drawing_contents=(fun _-> List.map (OutputCommon.translate 0. 0.) !contents)
                }]
          )]

    let footnote l=
      [Env (fun env->
              let next=match try snd (StrMap.find "footnotes" env.counters) with Not_found -> [] with
                  []->0
                | h::_->h
              in
                { env with counters=StrMap.add "footnotes" (-1,[next+1]) env.counters });
       bB (fun env0->
         let w0=env0.size in
         let env= { env0 with normalMeasure=env0.normalMeasure-.w0; normalLeftMargin=w0 } in
               let count=match try snd (StrMap.find "footnotes" env.counters) with Not_found -> [] with
                   []->0
                 | h::_->h
               in
               let foot_num=ref (-1) in
               let page_footnotes=ref 1 in
                 UserMap.iter (fun k a->
                                 match k with
                                     Footnote (i,_) when i= count -> foot_num:=a.page
                                   | _->()
                              ) (user_positions env);
                 (* Y a-t-il deja des footnotes sur cette page ? *)
                 UserMap.iter (fun k a->
                                 match k with
                                     Footnote (i,_) when a.page= !foot_num && i< count ->
                                       incr page_footnotes
                                   | _->()
                              ) (user_positions env);
                 (* Insertion d'une footnote *)
                 let str=ref (Node empty,[]) in

                 let params env a1 a2 a3 a4 a5 a6 line=
                   let p={(parameters env a1 a2 a3 a4 a5 a6 line) with min_height_after=0.} in
                   if not p.absolute && line.lineStart=0 then (
                     let rec findMark w j=
                       if j>=line.lineEnd then 0. else
                         if a1.(line.paragraph).(j) = User AlignmentMark then w else
                           let (_,ww,_)=box_interval a1.(line.paragraph).(j) in
                           findMark (w+.ww) (j+1)
                     in
                     let w=findMark 0. 0 in
                     { p with
                       left_margin=p.left_margin-.w;
                       measure=p.measure+.w }
                   ) else
                     p
                 in
                 let comp complete mes a1 a2 a3 a4 line a6=
                   if line.lineStart>0 then complete mes a1 a2 a3 a4 line a6 else (
                     let rec findMark w j=
                       if j>=Array.length a1.(line.paragraph) then 0. else
                         if a1.(line.paragraph).(j) = User AlignmentMark then w else
                           let (_,ww,_)=box_interval a1.(line.paragraph).(j) in
                           findMark (w+.ww) (j+1)
                     in
                     complete { mes with normalMeasure=mes.normalMeasure+.findMark 0. 0 } a1 a2 a3 a4 line a6
                   )
                 in

                   newPar str (comp normal) params
                     (tT (string_of_int !page_footnotes)
                      ::tT " "
                      ::bB (fun _->[User AlignmentMark])
                      ::l);
                   let a=1./.(sqrt phi) in
                   let pages=minipage { env with
                                          normalLead=env.lead*.a;
                                          lead=env.lead*.a;
                                          size=env.size*.a }
                     (top !str)
                   in
                     if Array.length pages>0 then
                       [User (Footnote (count, pages.(0)));
                        Drawing (drawing ~offset:(env.size/.2.)
                                   (draw_boxes env (boxify_scoped { env with size=env.size/.(sqrt phi) }
                                                  [tT (string_of_int !page_footnotes)])
                                   ))
                       ]
                     else
                       []
            )]

    let env_stack=ref []

    let displayedFormula a b c d e f g line=
      { (center a b c d e f g line) with
        min_height_before=3.*.a.lead/.4.;
        min_height_after=3.*.a.lead/.4.;
        not_first_line=true }

    module Env_center = struct

      let do_begin_env ()=
        D.structure:=newChildAfter (!D.structure) (Node empty);
        env_stack:=(List.map fst (snd !D.structure)) :: !env_stack

      let do_end_env ()=
        let center p = { p with par_parameters=Document.do_center p.par_parameters } in
        let res0, path0=(follow (top !D.structure) (List.rev (List.hd !env_stack))) in
        let res = map_paragraphs center res0 in
          D.structure:=up (res, path0);
          env_stack:=List.tl !env_stack

    end
    module Env_raggedRight = struct

      let do_begin_env ()=
        D.structure:=newChildAfter (!D.structure) (Node empty);
        env_stack:=(List.map fst (snd !D.structure)) :: !env_stack

      let do_end_env ()=
        let rag p = { p with par_parameters=ragged_right } in
        let res0, path0=(follow (top !D.structure) (List.rev (List.hd !env_stack))) in
        let res = map_paragraphs rag res0 in
          D.structure:=up (res, path0);
          env_stack:=List.tl !env_stack

    end
    module Env_raggedLeft = struct

      let do_begin_env ()=
        D.structure:=newChildAfter (!D.structure) (Node empty);
        env_stack:=(List.map fst (snd !D.structure)) :: !env_stack

      let do_end_env ()=
        let rag p = { p with par_parameters=ragged_left } in
        let res0, path0=(follow (top !D.structure) (List.rev (List.hd !env_stack))) in
        let res = map_paragraphs rag res0 in
          D.structure:=up (res, path0);
          env_stack:=List.tl !env_stack

    end

    let tiret_w env=phi*.env.size

    module type Enumeration=sig
      val from_counter:int list->content list
    end
    module Enumerate = functor (M:Enumeration)->struct
      let do_begin_env ()=
        D.structure:=newChildAfter (!D.structure) (Node empty);
        D.structure:=newChildAfter (!D.structure)
          (Node { empty with node_env=
               (fun env->
                  let lvl,cou=try StrMap.find "enumerate" env.counters with Not_found-> -1,[] in
                    { env with
                        normalMeasure=env.normalMeasure-.tiret_w env;
                        normalLeftMargin=env.normalLeftMargin+.tiret_w env;
                        counters=StrMap.add "enumerate" (lvl,(-1)::cou) env.counters }
               );
                    node_post_env=
               (fun env0 env1->
                  let cou=try
                    let lvl,enum=StrMap.find "enumerate" env1.counters in
                      StrMap.add "enumerate" (lvl,drop 1 enum) env1.counters
                  with Not_found-> env1.counters
                  in
                    { env0 with names=env1.names;user_positions=env1.user_positions;counters=cou })
                });
        env_stack:=(List.map fst (snd !D.structure)) :: !env_stack

      module Item=struct
        let do_begin_env ()=
          D.structure:=follow (top !D.structure) (List.rev (List.hd !env_stack));
          D.structure:=newChildAfter (follow (top !D.structure) (List.rev (List.hd !env_stack)))
            (Node { empty with
              node_tags=("item","")::empty.node_tags;
              node_env=(incr_counter "enumerate")
            })
        let do_end_env()=()
      end

      let do_end_env ()=
        let params parameters env a1 a2 a3 a4 a5 a6 line=
          let p=parameters env a1 a2 a3 a4 a5 a6 line in
            if not p.absolute && line.lineStart=0 then (
              let rec findMark w j=
                if j>=line.lineEnd then 0. else
                  if a1.(line.paragraph).(j) = User AlignmentMark then w else
                    let (_,ww,_)=box_interval a1.(line.paragraph).(j) in
                      findMark (w+.ww) (j+1)
              in
              let w=findMark 0. 0 in
                { p with
                    left_margin=p.left_margin-.w;
                    measure=p.measure+.w }
            ) else
              p
        in
        let comp complete mes a1 a2 a3 a4 line a6=
          if line.lineStart>0 then complete mes a1 a2 a3 a4 line a6 else (
            let rec findMark w j=
              if j>=Array.length a1.(line.paragraph) then 0. else
                if a1.(line.paragraph).(j) = User AlignmentMark then w else
                  let (_,ww,_)=box_interval a1.(line.paragraph).(j) in
                    findMark (w+.ww) (j+1)
            in
              complete { mes with normalMeasure=mes.normalMeasure+.findMark 0. 0 } a1 a2 a3 a4 line a6
          )
        in
        let is_first_par=ref false in
        let rec enumerate do_it t=match t with
            Node n when List.mem_assoc "item" n.node_tags && not do_it ->(
              is_first_par:=true;
              Node { n with children=IntMap.map (enumerate true) n.children }
            )
          | Node n when List.mem_assoc "item" n.node_tags -> Node n
          | Node n->Node { n with children=IntMap.map (enumerate do_it) n.children }
          | Paragraph p when do_it->
            let par_contents=
              if !is_first_par then (
                let item=bB (fun env->
                  let _,enum=try StrMap.find "enumerate" env.counters with Not_found->(-1),[0] in
                  let bb=boxify_scoped env (M.from_counter enum) in
                  let fix g= { g with drawing_min_width=g.drawing_nominal_width;
                    drawing_max_width=g.drawing_nominal_width }
                  in
                  let boxes=List.map (function Glue g->Glue (fix g) | Drawing g->Drawing (fix g) | x->x) bb in
                  boxes@[User AlignmentMark])
                in
                is_first_par:=false;
                item::p.par_contents
              ) else p.par_contents
            in
            Paragraph { p with
              par_parameters=params p.par_parameters;
              par_completeLine=comp p.par_completeLine;
              par_contents=par_contents
            }
          | _->t
        in
        D.structure:=follow (top !D.structure) (List.rev (List.hd !env_stack));
        let a,b= !D.structure in
        D.structure:=(enumerate false a,b);
        D.structure:=up (up !D.structure);
        env_stack:=List.tl !env_stack
    end

    module Env_itemize =
      Enumerate(struct
                  let from_counter _ =
                    [
                      bB (fun env->[Drawing (
                                     let y=env.size/.4. in
                                     let x0=tiret_w env/.phi in
                                     let x1=tiret_w env-.x0 in
                                       { drawing_min_width=tiret_w env;
                                         drawing_nominal_width=tiret_w env;
                                         drawing_max_width=tiret_w env;
                                         drawing_y0=y; drawing_y1=y;
                                         drawing_badness=(fun _->0.);
                                         drawing_contents=(fun _->
                                                             [OutputCommon.Path
                                                                ({OutputCommon.default with
                                                                    OutputCommon.lineWidth=0.1},
                                                                 [[|[|x0;x1|],[|y;y;|]|]])
                                                             ]) }
                                   )])
                    ]
                end)

    module type Enumerate_Pattern = sig
      val arg1 : char * (string -> content list)
    end

    module Env_genumerate = functor (Pat:Enumerate_Pattern) ->
      Enumerate(struct
	let c, f = Pat.arg1
	let g = match c with
	    '1' -> string_of_int
	  | 'a' -> Numerals.alphabetic ~capital:false
	  | 'A' -> Numerals.alphabetic ~capital:true
	  | 'i' -> Numerals.roman ~capital:false
	  | 'I' -> Numerals.roman ~capital:true
	  | _ -> failwith (Printf.sprintf "bad enumerate pattern &%c" c)
	let from_counter x =
	  let x = List.hd x + 1 in
	  f (g x)
      end)

    module Env_enumerate =
      Enumerate(struct
                  let from_counter x =
                    [ tT(string_of_int (List.hd x + 1));tT".";
                      bB (fun env->let w=env.size/.phi in [glue w w w])]
                end)

    module Env_abstract = struct

      let do_begin_env ()=
        D.structure:=newChildAfter !D.structure (Node empty);
        env_stack:=(List.map fst (snd !D.structure)) :: !env_stack


      let do_end_env () =
        D.structure :=
          up (change_env (follow (top !D.structure) (List.rev (List.hd !env_stack)))
                (fun x->{ x with
                            normalLeftMargin=(x.normalLeftMargin
                                              +.(x.normalMeasure-.120.)/.2.);
                            normalMeasure=120. }));
        env_stack:=List.tl !env_stack

    end

    module type Theorem=sig
      val refType:string
      val counter:string
      val counterLevel:int
      val display:string->content list
    end
    module Env_proof=struct
      let do_begin_env ()=
        D.structure:=newChildAfter !D.structure (Node empty);
        env_stack:=(List.map fst (snd !D.structure)) :: !env_stack

      let do_end_env ()=
        let par a b c d e f g line={ (ragged_right a b c d e f g line) with not_first_line=true;really_next_line=0 } in
        let bad env a b c d e f g h i j k l m=if d.isFigure then infinity else
          Document.badness env a b c d e f g h i j k l m
        in

        let par_proof a b c d e f g line=
          { (parameters a b c d e f g line) with min_height_before=if line.lineStart=0 then a.lead else 0. }
        in
        let cont=
          italic [tT "Proof.";bB (fun env->let w=env.size in [glue w w w])]
        in

        let rec add_proof t=match t with
            Node x->(
              try
                let a,b=IntMap.min_binding x.children in
                Node { x with children=IntMap.add a (add_proof b) x.children }
              with
                  Not_found->
                    let a=try fst (IntMap.min_binding x.children) with _->0 in
                    let par,_=(paragraph ~parameters:par_proof cont) in
                    Node { x with children=IntMap.add a par x.children}
            )
          | Paragraph p->
            Paragraph { p with
              par_contents=cont@p.par_contents;
              par_parameters=par_proof
            }
          | _->raise Not_found
        in

        D.structure:=(follow (top !D.structure) (List.rev (List.hd !env_stack)));
        D.structure:=up (add_proof (fst !D.structure), snd !D.structure);
        newPar D.structure ~badness:bad Complete.normal par
          [bB (fun env->
                let w=env.size/.phi in
                  [Drawing (
                     drawing [OutputCommon.Path ({ OutputCommon.default with
                                                     OutputCommon.close=true;
                                                     OutputCommon.lineWidth=0.1 },
                                                 [OutputCommon.rectangle (0.,0.) (w,w)]
                                                )])
                  ]
             )];
        env_stack:=List.tl !env_stack
    end
    module Proof = Env_proof (* probably useless, just for compatibility *)

    module Make_theorem=functor (Th:Theorem)->struct

      let reference name=generalRef Th.refType name

      let do_begin_env ()=
        D.structure:=newChildAfter !D.structure (Node empty);
        env_stack:=(List.map fst (snd !D.structure)) :: !env_stack

      let do_end_env ()=
        let rec last_par=function
            Paragraph p->
              Paragraph { p with
                            par_parameters=(fun a b c d e f g line->
                                              { (p.par_parameters a b c d e f g line) with
                                                  min_height_after=
                                                  if line.lineEnd>=Array.length b.(line.paragraph) then a.lead else 0. });
                        }
          | Node n->(try
                       let k0,a0=IntMap.max_binding n.children in
                       Node { n with children=IntMap.add k0 (last_par a0) n.children }
                     with Not_found -> Node n)
          | x -> x
        in
        let stru,path=follow (top !D.structure) (List.rev (List.hd !env_stack)) in



        let cont=
          Env (incr_counter ~level:Th.counterLevel Th.counter)::
            C (fun env->
              let lvl,num=try (StrMap.find Th.counter env.counters) with
                  Not_found -> -1,[0]
              in
              let _,str_counter=try
                                  StrMap.find "_structure" env.counters
                with Not_found -> -1,[0]
              in
              let sect_num=drop (max 1 (List.length str_counter - lvl+1))
                str_counter
              in
              Th.display (String.concat "." (List.map (fun x->string_of_int (x+1)) ((List.rev sect_num)@num)))
            )::
            [tT " "]
        in
        let rec add_name t=match t with
            Node x->(
              try
                let a,b=IntMap.min_binding x.children in
                Node { x with children=IntMap.add a (add_name b) x.children }
              with
                  Not_found->
                    let a=try fst (IntMap.min_binding x.children) with _->0 in
                    let par,_=paragraph cont in
                    Node { x with children=IntMap.add a par x.children}
            )
          | Paragraph p->
            Paragraph { p with
              par_parameters=(fun a b c d e f g h->
                let p=p.par_parameters a b c d e f g h in
                if h.lineStart=0 then {p with min_height_before=a.lead} else p);
              par_contents=cont@p.par_contents
            }
          | _->raise Not_found
        in
	D.structure := up (last_par (add_name stru),path);
        env_stack:=List.tl !env_stack
    end

    module Output(M:OutputPaper.Driver)=struct
      (** Output routines. An output routine is just a functor taking a driver module *)
      open OutputPaper
      open OutputCommon
      module type Driver=OutputPaper.Driver
      type output={
        mutable format:Box.box array array->(Document.tree*Document.cxt) array->
                       Box.drawingBox array->(Document.tree*Document.cxt) array->
                       (Line.parameters*Line.line) list -> OutputPaper.page;
        mutable pageNumbers:OutputPaper.page->environment->int->unit
      }
      let max_iterations=ref 3
      let outputParams={
        format=(fun _ _ _ _ _->{ pageFormat=a4; pageContents=[] });
        pageNumbers=(fun page env n->
          let pnum=glyph_of_string env.substitutions env.positioning env.font env.size env.fontColor (string_of_int (n+1)) in
          let (_,w,_)=boxes_interval (Array.of_list pnum) in
          let x=(fst page.pageFormat -. w)/.2. in
          let _=List.fold_left (fun x0 b->match b with
              (GlyphBox a)->(
                let (_,w,_)=box_interval b in
                page.pageContents<- (OutputCommon.Glyph { a with glyph_x=x0;glyph_y=20. })
                :: page.pageContents;
                x0+.w
               )
            | _ -> x0
          ) x pnum
          in
          ()
        )
      }
      let basic_output out_params tree defaultEnv file=
        let rec resolve i env0=
          Printf.printf "Compilation %d\n" i; flush stdout;
          let fixable=ref false in
          let env1,fig_params,params,compl,badness,paragraphs,paragraph_trees,figures,figure_trees=flatten env0 fixable tree in
          Printf.fprintf stderr "Début de l'optimisation : %f s\n" (Sys.time ());
          let (logs,opt_pages,figs',user')=TS.typeset
            ~completeLine:compl
            ~figure_parameters:fig_params
            ~figures:figures
            ~parameters:params
            ~badness:badness
            paragraphs
          in
          Printf.fprintf stderr "Fin de l'optimisation : %f s\n" (Sys.time ());
          let env, reboot=update_names env1 figs' user' in
          let env=reset_counters env in
          if i < !max_iterations-1 && reboot && !fixable then (
            resolve (i+1) env
          ) else (

            List.iter (fun x->Printf.fprintf stderr "%s\n" (Typography.TypoLanguage.message x)) logs;

            let positions=Array.make (Array.length paragraphs) (0,0.,0.) in
            let pages=Array.make (Array.length opt_pages) { pageFormat=0.,0.;pageContents=[] } in
            let par=ref (-1) in
            let crosslinks=ref [] in (* (page, link, destination) *)
            let crosslink_opened=ref false in
            let destinations=ref StrMap.empty in
            let urilinks=ref None in

            let continued_link=ref None in

            let draw_page i p=
              pages.(i)<-out_params.format paragraphs paragraph_trees figures figure_trees p;
              let page=pages.(i) in
              let footnotes=ref [] in
              let footnote_y=ref (-.infinity) in
              let pp=Array.of_list p in
              let w,h=page.pageFormat in

              let endlink cont=
                continued_link:=None;
                if !crosslink_opened then (
                  let rec link_contents u l=match l with
                      []->[]
                    | (Link h)::s->(
                      if cont then continued_link:=Some (Link h);
                      Link { h with link_contents=List.rev u }
                    )::s
                    | h::s->link_contents (h::u) s
                  in
                  page.pageContents<-link_contents [] page.pageContents;
                  (match !urilinks with
                      None->(
                        match !crosslinks with
                            []->()
                          | (_,h,_)::s->crosslink_opened:=false
                      )
                    | Some h->(
                      page.pageContents<-Link h::page.pageContents;
                      urilinks:=None;
                    )
                  )
                )
              in

              (match !continued_link with
                  None->()
                | Some l->(
                  page.pageContents<-l::page.pageContents;
                  crosslink_opened:=true;
                  continued_link:=None
                )
              );

              for j=0 to Array.length pp-1 do
                let topMargin=(h-.(fst pp.(0)).page_height)/.2. in
                let param,line=pp.(j) in
                let y=h-.topMargin-.line.height in

                if line.isFigure then (
                  let fig=figures.(line.lastFigure) in
                  let y=
                    if j>0 && j<Array.length pp-1 then
                      let milieu=
                        (h-.topMargin-.(snd pp.(j-1)).height+.fst (line_height paragraphs figures (snd pp.(j-1)))
                         +.(h-.topMargin-.(snd pp.(j+1)).height+.snd (line_height paragraphs figures (snd pp.(j+1)))))/.2.
                      in
                      milieu-.(fig.drawing_y1+.fig.drawing_y0)/.2.
                    else
                      y
                  in
	          if env.show_boxes then
                    page.pageContents<- Path ({OutputCommon.default with close=true;lineWidth=0.1 },
                                              [rectangle (param.left_margin,y+.fig.drawing_y0)
                                                  (param.left_margin+.fig.drawing_nominal_width,
                                                   y+.fig.drawing_y1)]) :: page.pageContents;
                  page.pageContents<- (List.map (translate param.left_margin y)
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
		        if env.show_boxes then
                          page.pageContents<- Path ({OutputCommon.default with close=true;lineWidth=0.1 }, [rectangle (x,y+.g.drawing_y0) (x+.w,y+.g.drawing_y1)]) :: page.pageContents;
                        w
                      )
                      | User (BeginURILink l)->(
                        let link={ link_x0=x;link_y0=y;link_x1=x;link_y1=y;uri=l;
                                   dest_page=(-1);dest_x=0.;dest_y=0.;is_internal=false;
                                   link_contents=[] }
                        in
                        urilinks:=Some link;
                        page.pageContents<-Link link::page.pageContents;
                        0.
                      )
                      | User (BeginLink l)->(
                        let link={ link_x0=x;link_y0=y;link_x1=x;link_y1=y;uri=l;
                                   dest_page=line.Line.page;dest_x=0.;dest_y=0.;is_internal=true;
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
                        endlink false;
                        0.
                      )
                      | User (Footnote (_,g))->(
                        footnotes:= g::(!footnotes);
                        footnote_y:=max !footnote_y (h-.topMargin-.param.page_height);
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
                  endlink true;
                  (match !continued_link with
                      None->()
                    | Some l->(
                      page.pageContents<-l::page.pageContents;
                      crosslink_opened:=true;
                      continued_link:=None
                    )
                  );

                  (match !urilinks with
                      None->()
                    | Some h->h.link_x1<-x1);
                  if !crosslink_opened then
                    (match !crosslinks with
                        []->()
                      | (_,h,_)::_->h.link_x1<-x1)
                )
              done;

              endlink true;

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
              out_params.pageNumbers page env i;
              page.pageContents<-List.rev page.pageContents
            in
            for i=0 to Array.length pages-1 do draw_page i opt_pages.(i) done;

            let pages=Array.map (fun p->
              { p with
                pageContents=List.map (fun a->match a with
                    Link l when l.is_internal->(
                      try
                        let (p',x,y0,y1)=StrMap.find l.uri !destinations in
                        let dx0,dy0,dx1,dy1=bounding_box l.link_contents in
                        Link { l with dest_page=p'; dest_x=x; dest_y=y0+.(y1-.y0)*.phi;
                          link_x0=dx0;link_x1=dx1;
                          link_y0=dy0;link_y1=dy1
                             }
                      with
                          Not_found->a
                    )
                  | a->a
                ) p.pageContents
              }
            ) pages
            in

            (* List.iter (fun (p,link,dest)->try *)
            (*                                 let (p',x,y0,y1)=StrMap.find dest !destinations in *)
            (*                                 pages.(p).pageContents<-Link { link with dest_page=p'; dest_x=x; dest_y=y0+.(y1-.y0)*.phi } *)
            (*                                 ::pages.(p).pageContents *)
            (*   with *)
            (*       Not_found->() *)
            (* ) !crosslinks; *)


            M.output ~structure:(make_struct positions tree) pages file
          )
        in
        resolve 0 defaultEnv

      let output out_params structure defaultEnv file=
        basic_output out_params (postprocess_tree structure) defaultEnv file
    end
  end)

module MathFonts = struct
  let asana_font=Lazy.lazy_from_fun (fun ()->Typography.Fonts.loadFont (Typography.Util.findFont "Asana-Math/Asana-Math.otf"))
  let asana name code = Maths.symbol ~name (Lazy.force asana_font) [code]

  let euler_font=Lazy.lazy_from_fun (fun ()->Typography.Fonts.loadFont (Typography.Util.findFont "Euler/euler.otf"))
  let euler name code = Maths.symbol ~name (Lazy.force euler_font) [code]

  let ams_font=Lazy.lazy_from_fun (fun ()->Typography.Fonts.loadFont (Typography.Util.findFont "AMS/ams.otf"))
  let ams name code = Maths.symbol ~name (Lazy.force ams_font) [code]
end



module MathsFormat=struct
    (* Symboles et polices de maths *)

    module MathFonts = MathFonts
    let mathsText t0=
      [Maths.Ordinary (Maths.noad (fun env st->boxify_scoped
        { env with size=env.size*.(Maths.env_style env.mathsEnvironment st).Mathematical.mathsSize }
        t0 ))]
    let mathcal a=[Maths.Scope(fun _ _-> Maths.Env (Euler.changeFont [Euler.Font `Cal]):: a)]
    let cal a=mathcal a
    let fraktur a=[Maths.Scope(fun _ _-> Maths.Env (Euler.changeFont [Euler.Font `Fraktur]) :: a)]
    let mathbf a=[Maths.Scope(fun _ _-> Maths.Env (fun env -> Euler.changeFont [Euler.Graisse `Gras] (envAlternative [] Bold env)) :: a)]
    let mathsc a=
      [Maths.Scope(fun _ _->
                     Maths.Env (fun env->envAlternative [] Caps env)::
                       Maths.Env (fun env->Maths.change_fonts env env.font)::
                       a
                  )]

    let bbFont=Lazy.lazy_from_fun (fun ()->Fonts.loadFont (findFont "AMS/ams.otf"))
    let mathbb a=[Maths.Scope (fun _ _->Maths.Env (fun env->Maths.change_fonts 
                                                     (change_font (Lazy.force bbFont) env) (Lazy.force bbFont))::a)]

    let mathrm a=[Maths.Scope(
                    fun _ _->Maths.Env (fun env->Maths.change_fonts env env.font)::a
                  )]

    let displayStyle a=[Maths.Scope(
      fun _ _->Maths.Env (fun env-> { env with mathStyle
        = Mathematical.Display })::a
    )]

    let oline a=
      [Maths.Ordinary
         (Maths.noad
            (fun envs st->
               let dr=draw_boxes envs (Maths.draw [envs] a) in
               let env=Maths.env_style envs.mathsEnvironment st in
               let (x0,y0,x1,y1)=OutputCommon.bounding_box dr in
               let drawn=(drawing ~offset:y0 dr) in
               let rul=(env.Mathematical.default_rule_thickness)*.env.Mathematical.mathsSize in
                 [Box.Drawing {
                    drawn with
                      drawing_y1=drawn.drawing_y1*.sqrt phi+.rul;
                      drawing_contents=
                      (fun w->
                         OutputCommon.Path ({OutputCommon.default with OutputCommon.lineWidth=rul},
                                            [[|[|x0;x1|],
                                               [|y1*.sqrt phi+.2.*.rul;y1*.sqrt phi+.2.*.rul|]|]])
                         ::drawn.drawing_contents w)
                  }]
            ))]

    let binomial a b=
      [Maths.Fraction { Maths.numerator=b; Maths.denominator=a;
                        Maths.line=(fun _ _->{OutputCommon.default with OutputCommon.fillColor=None;OutputCommon.strokingColor=None}) }]

    (* Une chirurgie esthétique de glyphs. Ce n'est sans doute pas très
       bien fait, et il faut kerner en haut. Un truc generique pour
       allonger toutes les flêches est à réfléchir *)

    let oRightArrow a=
      [Maths.Ordinary
         (Maths.noad
            (fun envs st->
               let boxes=(Maths.draw [envs] a) in
               let boxes_w=
                 (List.fold_left (fun w x->
                                    let _,w_x,_=box_interval x in
                                      w+.w_x) 0. boxes)
               in
               let dr=draw_boxes envs boxes in
               let (x0_,y0_,x1_,y1_)=OutputCommon.bounding_box dr in

               let env=Maths.env_style envs.mathsEnvironment st in
               let font=Lazy.force (env.Mathematical.mathsFont) in
               let utf8_arr={glyph_index=(Fonts.glyph_of_uchar font (UChar.chr 0x2192));
                             glyph_utf8="\033\146"} in
               let gl_arr=Fonts.loadGlyph font utf8_arr in
               let arr=Fonts.outlines gl_arr in
               let w1=List.fold_left (List.fold_left (fun y (v,_)->max y (max v.(0) v.(Array.length v-1)))) 0. arr in
               let y0,y1=List.fold_left (List.fold_left (fun (yy0,yy1) (_,v)->
                                                           let a,b=Bezier.bernstein_extr v in
                                                             min yy0 a, max yy1 b)) (0.,0.) arr in
               let size=envs.size*.env.Mathematical.mathsSize/.(1000.*.phi) in
               let space=env.Mathematical.default_rule_thickness in
               let arr'=
                 List.map (fun x->
                             Array.of_list (List.map (fun (u,v)->
                                                        Array.map (fun y->if y>=w1/.4. then (y*.size)+.(max 0. (x1_-.w1*.size)) else y*.size) u,
                                                        Array.map (fun y->y*.size-.y0+.y1_+.space) v
                                                     ) x)) arr
               in
                 [Box.Drawing {
                    drawing_nominal_width=max (w1*.size) boxes_w;
                    drawing_min_width=max (w1*.size) boxes_w;
                    drawing_max_width=max (w1*.size) boxes_w;

                    drawing_y0=y0_;
                    drawing_y1=y1_+.space-.(y0+.y1)*.size;
                    drawing_badness=(fun _->0.);
                    drawing_contents=
                      (fun w->
                         OutputCommon.Path ({OutputCommon.default with
                                               OutputCommon.strokingColor=None;
                                               OutputCommon.fillColor=Some OutputCommon.black
                                            },arr')
                         ::(List.map (OutputCommon.translate (max 0. ((w1*.size-.x1_)/.2.)) 0.) dr))
                  }]
            ))]

    let oLeftArrow a=
      [Maths.Ordinary
         (Maths.noad
            (fun envs st->
               let boxes=(Maths.draw [envs] a) in
               let boxes_w=
                 (List.fold_left (fun w x->
                                    let _,w_x,_=box_interval x in
                                      w+.w_x) 0. boxes)
               in
               let dr=draw_boxes envs boxes in
               let (x0_,y0_,x1_,y1_)=OutputCommon.bounding_box dr in

               let env=Maths.env_style envs.mathsEnvironment st in
               let font=Lazy.force (env.Mathematical.mathsFont) in
               let utf8_arr={glyph_index=(Fonts.glyph_of_uchar font (UChar.chr 0x2190));
                             glyph_utf8="\033\144"} in
               let gl_arr=Fonts.loadGlyph font utf8_arr in
               let arr=Fonts.outlines gl_arr in
               let w1=List.fold_left (List.fold_left (fun y (v,_)->max y (max v.(0) v.(Array.length v-1)))) 0. arr in
               let y0,y1=List.fold_left (List.fold_left (fun (yy0,yy1) (_,v)->
                                                           let a,b=Bezier.bernstein_extr v in
                                                             min yy0 a, max yy1 b)) (0.,0.) arr in
               let size=envs.size*.env.Mathematical.mathsSize/.(1000.*.phi) in
               let space=env.Mathematical.default_rule_thickness in
               let arr'=
                 List.map (fun x->
                             Array.of_list (List.map (fun (u,v)->
                                                        Array.map (fun y->if y>=w1*.0.75 then (y*.size)+.(max 0. (x1_-.w1*.size)) else y*.size) u,
                                                        Array.map (fun y->y*.size-.y0+.y1_+.space) v
                                                     ) x)) arr
               in
                 [Box.Drawing {
                    drawing_nominal_width=max (w1*.size) boxes_w;
                    drawing_min_width=max (w1*.size) boxes_w;
                    drawing_max_width=max (w1*.size) boxes_w;

                    drawing_y0=y0_;
                    drawing_y1=y1_+.space-.(y0+.y1)*.size;
                    drawing_badness=(fun _->0.);
                    drawing_contents=
                      (fun w->
                         OutputCommon.Path ({OutputCommon.default with
                                               OutputCommon.strokingColor=None;
                                               OutputCommon.fillColor=Some OutputCommon.black
                                            },arr')
                         ::(List.map (OutputCommon.translate (max 0. ((w1*.size-.x1_)/.2.)) 0.) dr))
                  }]
            ))]

    let vec = oRightArrow

    let cev = oLeftArrow
        (*******************************************************)

end
