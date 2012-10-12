open Typography
open Typography.Document
open Typography.OutputCommon
open Typography.OutputPaper
open Typography.Util

open Typography.Box
open Typography.Fonts
open Typography.Fonts.FTypes
open CamomileLibrary
open Typography.Line
open Typography.Document

module MathFonts=DefaultFormat.MathFonts
module MathsFormat=DefaultFormat.MathsFormat

let slideh=100.
let slidew=phi*.slideh


type state={
  subautomata:state IntMap.t;
  stateContents:(float*float) IntMap.t;
  transitions:(string StrMap.t)
}

module Format=functor (D:Document.DocumentStructure)->(
  struct

    module Default=DefaultFormat.Format(D)
    include (Default:module type of Default with module Output:=Default.Output)

    let mes=(slidew/.2.)*.phi
    let defaultEnv:user environment={
      Default.defaultEnv with
        normalMeasure=mes;
        normalLeftMargin=(slidew-.mes)/.2.;
        par_indent=[];
    }

    (* in slides, werbatim with a smaller lead *)
    let verbEnv x =
	{ (envFamily x.fontMonoFamily x)
	with size = x.size *. x.fontMonoRatio; normalMeasure=infinity; par_indent = [];
	     lead = x.lead *. x.fontMonoRatio *. 0.75}

    let parameters a b c d e f g line=
      { (Default.parameters a b c d e f g line) with
        page_height=2.*.slideh;
        next_acceptable_height=
        (fun a b c d e->e)

      }


    module type Title = sig
      val arg1 : (user content list)
    end

    module Env_slide=struct
      module Title=functor (M:Title)->(
        struct
          let do_begin_env ()=
            let res0, path0=(follow (top !D.structure) (List.rev (List.hd !env_stack))) in
            match res0 with
                Node node->
                  D.structure:=follow (top (Node {node with displayname=size 1.2 (bold M.arg1);name=string_of_contents M.arg1},path0)) (List.rev (List.map fst (snd !D.structure)))
              | _->assert false
          let do_end_env ()=()
        end
      )
      let do_begin_env ()=
        D.structure:=newChildAfter !D.structure (Node {empty with
          node_tags=("slide","")::empty.node_tags;
          node_env=(incr_counter "slide")
        });
        env_stack:=(List.map fst (snd !D.structure)) :: !env_stack;
        D.structure:=lastChild !D.structure

      let do_end_env ()=
        let slide=follow (top !D.structure) (List.rev (List.hd !env_stack)) in
        D.structure:=up slide;
        env_stack:=List.tl !env_stack
    end

    module type States=sig
      val arg1 : int list
    end
    module Env_states (S:States)=struct
      let do_begin_env ()=
        D.structure:=newChildAfter !D.structure (Node empty);
        env_stack:=(List.map fst (snd !D.structure)) :: !env_stack
      let do_end_env ()=
        let rec restate st t=match t with
            Paragraph p->Paragraph { p with
              par_states=
                if IntSet.is_empty p.par_states then st
                else IntSet.inter p.par_states st
            }
          | Node n->Node { n with
            children=IntMap.map (restate
                                   (if IntSet.is_empty n.node_states then st
                                    else IntSet.inter n.node_states st)
            ) n.children
          }
          | _->t
        in
        let states=List.fold_left (fun s x->IntSet.add x s) IntSet.empty S.arg1 in
        let slide,path=follow (top !D.structure) (List.rev (List.hd !env_stack)) in
        D.structure:=up (restate states slide,path);
        env_stack:=List.tl !env_stack
    end


    module type Driver'=sig
      val output': ?structure:structure -> (page array) array -> string -> unit
    end


    module Output(M:Driver')=struct

      type output={
        format:float*float;
      }
      let max_iterations=ref 3
      let outputParams=
        {
          format=slidew,slideh;
        }


      let output out_params structure defaultEnv file=
        let rec resolve i env0=
          Printf.printf "Compilation %d\n" i; flush stdout;
          let tree=structure in
          let logs=ref [] in
          let slides=ref [] in
          let reboot=ref false in
          let toc=ref [] in
          let rec typeset_structure path tree env0=
            if List.length path=1 then (
              match tree with
                  Node n when List.mem_assoc "InTOC" n.node_tags->(
                    toc:=(n.name,n.displayname,env0)::(!toc)
                  )
                | _->()
            );
            match tree with
                Node n when List.mem_assoc "slide" n.node_tags ->(
                  let out=open_out (Printf.sprintf "slide%d" (List.length !slides)) in
                  doc_graph out tree;
                  close_out out;

                  let rec get_max_state t=match t with
                      Paragraph p->(try IntSet.max_elt p.par_states with Not_found -> 0)
                    | Node n->
                      IntMap.fold (fun _ a m->max m (get_max_state a)) n.children
                        (try IntSet.max_elt n.node_states with Not_found->0)
                    | _->0
                  in
                  let max_state=get_max_state tree in

                  let fixable=ref false in
                  let env=n.node_env env0 in
                  let env1,fig_params0,params0,compl0,badnesses0,paragraphs0,_,
                    figures0,figure_trees0=flatten env fixable tree
                  in

                  let opts=Array.make (max_state+1) [] in

                  (* Typesetting de tous les états *)

                  let rec typeset_states state reboot_ env=
                    if state>max_state then (reboot_,env) else (

                      let real_par=ref 0 in
                      let par_map=ref IntMap.empty in
                      let rec make_paragraphs t=match t with
                          Paragraph p when IntSet.mem state p.par_states
                              || IntSet.is_empty p.par_states -> (
                          (* Celui-ci, on le garde *)
                                let x=try fst (IntMap.max_binding !par_map) with Not_found -> (-1) in
                                par_map:=IntMap.add (x+1) !real_par !par_map;
                                incr real_par
                              )
                        | Paragraph p->(
                          incr real_par
                        )
                        | Node n->IntMap.iter (fun _ a->make_paragraphs a) n.children
                      in
                      make_paragraphs tree;
                      let fig_params=[||] and figures=[||] in
                      let params=
                        if IntMap.is_empty !par_map then [||] else
                          Array.make (IntMap.cardinal !par_map) params0.(0)
                      and paragraphs=
                        if IntMap.is_empty !par_map then [||] else
                          Array.make (IntMap.cardinal !par_map) paragraphs0.(0)
                      and compl=
                        if IntMap.is_empty !par_map then [||] else
                          Array.make (IntMap.cardinal !par_map) compl0.(0)
                      and badnesses=
                        if IntMap.is_empty !par_map then [||] else
                          Array.make (IntMap.cardinal !par_map) badnesses0.(0)
                      in
                      IntMap.iter (fun k a->
                        params.(k)<-params0.(a);
                        paragraphs.(k)<-paragraphs0.(a);
                        compl.(k)<-compl0.(a);
                        badnesses.(k)<-badnesses0.(a);
                      ) !par_map;
                      let (logs_,opt_pages,figs',user')=TS.typeset
                        ~completeLine:compl
                        ~figure_parameters:fig_params
                        ~figures:figures
                        ~parameters:params
                        ~badness:badnesses
                        paragraphs
                      in
                      opts.(state)<-
                        List.map (fun (param,parag)->
                          (param, { parag with paragraph=IntMap.find parag.paragraph !par_map })
                        ) (if Array.length opt_pages>0 then opt_pages.(0) else []);
                      (* let env2,reboot'=update_names env1 figs' user' in *)
                      (* typeset_states (state+1) (reboot_ || (reboot'&& !fixable)) env2 *)
                      typeset_states (state+1) reboot_ env1
                    )
                  in

                  let reboot',env'=typeset_states 0 !reboot env in
                  reboot:=reboot';

                (* On colle chaque paragraphe le plus bas parmi toutes
                   les possibilités rencontrées. *)
                (* Position verticale de la première ligne de chaque
                   paragraphe *)
                  let par_current=Array.make_matrix (Array.length opts)
                    (Array.length paragraphs0) infinity
                  in
                  for i=0 to Array.length opts-1 do
                    List.iter (fun (_,line)->
                      if line.lineStart=0 then (
                        par_current.(i).(line.paragraph)<-line.height;
                      )
                    ) opts.(i)
                  done;

                (* offs.(i) est la différence courante, dans l'état i,
                   entre la hauteur calculée par l'optimiseur et la
                   vraie hauteur, pour tous les paragraphes placés
                   jusque là. *)
                  let offs=Array.make (Array.length opts) 0. in

                (* par_pos.(p) est la position définitive du paragraphe p. *)
                  let par_pos=Array.make (Array.length paragraphs0) 0. in

                  for par=0 to Array.length paragraphs0-1 do

                  (* Calculer les offsets après avoir placé le
                     paragraphe par sur tous les états où il apparaît. *)
                    let rec max_off i off=
                      if i>=Array.length opts then off else (
                        if par_current.(i).(par)<>infinity then
                          max_off (i+1) (max (par_current.(i).(par)+.offs.(i)) off)
                        else
                          max_off (i+1) off
                      )
                    in
                    par_pos.(par)<-max_off 0 0.;
                  (* Mise à jour des nouvelles positions définitives *)
                    for i=0 to Array.length opts-1 do
                      if par_current.(i).(par)<>infinity then
                        offs.(i)<-offs.(i)+.(par_pos.(par)-.par_current.(i).(par))
                    done
                  done;

                  (*  *)
                  for i=0 to Array.length opts-1 do
                    opts.(i)<-List.map
                      (fun (a,b)->(a,{ b with
                        height=b.height+.par_pos.(b.paragraph)-.par_current.(i).(b.paragraph)
                      }))
                      opts.(i)
                  done;

                  (* Centrage collectif sur la page, et interligne *)
                  let linegap=
                    env.normalLead/.(phi*.phi)
                  in
                  let rec make_lineGaps h l=match l with
                      []->[]
                    | (p,u)::v->(p,{ u with height=u.height+.h })::
                      (make_lineGaps (h+.linegap) v)
                  in
                  let rec extrema m0 m1 l=match l with
                      []->if m0=infinity then 0.,0. else m0,m1
                    | (_,h)::s->
                      let m0',m1'=Box.line_height paragraphs0 figures0 h in
                      extrema (min m0 (h.height+.m0')) (max m1 (h.height+.m1')) s
                  in
                  let text_h states=
                    let m0=ref infinity and m1=ref (-.infinity) in
                    for i=0 to Array.length states-1 do
                      let m0',m1'=extrema !m0 !m1 states.(i) in
                      m0:=m0';m1:=m1'
                    done;
                    let m0= !m0 and m1= !m1 in
                    let h=max 0. (slideh-.(m1-.m0))/.2. in
                    Array.map (List.map (fun (p,l)->p,{l with height=l.height+.h})) states
                  in
                  let opts=Array.map (make_lineGaps 0.) opts in
                  let opts=text_h opts in
                  (* Fini *)


                  slides:=(path,tree,paragraphs0,figures0,figure_trees0,env,opts)::(!slides);
                  n.node_post_env env0 env'
                )
              | Node n->
                n.node_post_env env0 (
                  IntMap.fold (fun k a m->typeset_structure (k::path) a m) n.children
                    (n.node_env env0)
                )
              | Paragraph p->p.par_post_env env0 (p.par_env env0)
              | FigureDef f->f.fig_post_env env0 (f.fig_env env0)
          in
          Printf.fprintf stderr "Début de l'optimisation : %f s\n" (Sys.time ());
          let env=typeset_structure [] tree defaultEnv in
          Printf.fprintf stderr "Fin de l'optimisation : %f s\n" (Sys.time ());

          if i < !max_iterations-1 && !reboot then (
            resolve (i+1) env
          ) else (


            let toc=List.fold_left (fun m (n,dis,env)->
              let a,b=try StrMap.find "_structure" env.counters with Not_found -> -1,[0] in
              IntMap.add (List.hd b) (dis,env) m;
            ) IntMap.empty !toc
            in

            let y_menu=slideh-.env.size in

            let draw_toc env0=
              let _,b0=try StrMap.find "_structure" env0.counters with Not_found -> -1,[0] in
              let boxes=IntMap.map (fun (displayname,env)->
                let _,b=try StrMap.find "_structure" env.counters with Not_found -> -1,[0] in
                let rec prefix u v=match u,v with
                    [],_->true
                  | hu::_,hv::_ when hu<>hv->false
                  | _::su,_::sv->prefix su sv
                  | _,[]->false
                in
                let col=if (prefix (List.rev b) (List.rev b0)) then white else gray in
                boxify_scoped { env with fontColor=col } displayname
              ) toc
              in
              let total=IntMap.fold (fun _ a m->
                List.fold_left (fun w x->let _,w',_=box_interval x in w+.w') m a
              ) boxes 0.
              in
              let alpha=(min 1.0 (slidew*.0.8/.total))/.(sqrt phi) in
	      let denom, start_space = if IntMap.cardinal toc > 1 && total > slidew *. 0.5 then
		 (* espace fixe au bord à 0.125 slidew *) 
		  (float_of_int (IntMap.cardinal toc - 1)),
  		  (fun inter -> 0.125 *. slidew)
		else
		  (float_of_int (IntMap.cardinal toc + 1)),
  		  (fun inter -> inter +. 0.125 *. slidew)
	      in
              let inter=(0.75 *. slidew -. (alpha*.total))/.denom in
              let x0=slidew/.10.
              and y0=slideh
              and x1=slidew*.0.9
              and y1=slideh-.env.size*.phi
              and r=2.
              in
              let lambda=r*.4.*.(sqrt 2.-.1.)/.3. in
              let rect=
                [|[|x0;x0|],[|y0;y1+.r|];
                  [|x0;x0;x0+.r-.lambda;x0+.r|],[|y1+.r;y1+.r-.lambda;y1;y1|];
                  [|x0+.r;x1-.r|],[|y1;y1|];
                  [|x1-.r;x1-.r+.lambda;x1;x1|],[|y1;y1;y1+.r-.lambda;y1+.r|];
                  [|x1;x1|],[|y1+.r;y0|]
                |]
              in
              let cont=ref [
                Path ({default with fillColor=Some black},
                      [rect])
              ]
              in
              let drawn=IntMap.fold (fun _ a m->
                cont:=(List.map (fun x->translate m y_menu (OutputCommon.resize alpha x))
                         (draw_boxes a))@(!cont);
                let w=List.fold_left (fun w x->let _,w',_=box_interval x in w +. alpha *. w') 0. a in
                m+.inter+.w
              ) boxes (start_space inter)
              in
              !cont
            in
            let draw_slide (path,tree,paragraphs,figures,figure_trees,env,opts)=
              let states=ref [] in
              for st=0 to Array.length opts-1 do
                let page={ pageFormat=slidew,slideh; pageContents=[] } in
                page.pageContents<-draw_toc env;

                let tit=
                  draw_boxes (boxify_scoped {env with size=0.1} (match tree with
                      Node n->n.displayname
                    | _->[]))
                in
                let (_,yy0,_,yy1)=bounding_box tit in
                let y1=slideh-.env.size*.2. in

                page.pageContents<-(List.map (translate (slidew/.8.) (y1-.phi*.(yy1-.yy0))) tit)@page.pageContents;
                let pp=Array.of_list opts.(st) in
                let w,h=slidew,slideh in
                for j=0 to Array.length pp-1 do
                  let param,line=pp.(j) in
                  let y=h-.line.height in

                  if line.isFigure then (
                    let fig=figures.(line.lastFigure) in
	            if env.show_boxes then
                      page.pageContents<- Path ({OutputCommon.default with close=true;lineWidth=0.1 },
                                                [rectangle (param.left_margin,y+.fig.drawing_y0)
                                                    (param.left_margin+.fig.drawing_nominal_width,
                                                     y+.fig.drawing_y1)]) :: page.pageContents;
                    page.pageContents<- (List.map (translate param.left_margin y)
                                           (fig.drawing_contents fig.drawing_nominal_width))
                    @ page.pageContents;

                  ) else if line.paragraph<Array.length paragraphs then (

                    let comp=compression paragraphs param line in
                    let rec draw_box x y box=
                      let lowy=y+.lower_y box in
                      let uppy=y+.upper_y box in
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
                        | b->box_width comp b
                    in
                    let x1=fold_left_line paragraphs (fun x b->x+.draw_box x y b) param.left_margin line in
                    ()
                  )
                done;
                page.pageContents<-List.rev page.pageContents;
                states:=page:: !states
              done;
              env,Array.of_list (List.rev !states)
            in
            let pages=Array.of_list (List.map draw_slide (List.rev !slides)) in
            let slide_num=ref 0 in
            let rec make_structure t=
              match t with
                  Node n when List.mem_assoc "slide" n.node_tags ->(
                    let sl=
                      let open Typography.OutputCommon in
                          {name=(try List.assoc "name" n.node_tags with Not_found->"");
                           displayname=[];metadata=[];tags=n.node_tags;
		           page= !slide_num;struct_x=0.;struct_y=0.;
                           substructures=[||]
                          }
                    in
                    incr slide_num;
                    sl
                  )
                | Node n when List.mem_assoc "InTOC" n.node_tags->
                  let sub=IntMap.fold (fun _ a m->
                    match a with
                        Node _ when List.mem_assoc "InTOC" n.node_tags->(make_structure a)::m
                      | _->m
                  ) n.children []
                  in
                  let open Typography.OutputCommon in
                  {name=(try List.assoc "name" n.node_tags with Not_found->"");
                   displayname=[];metadata=[];tags=n.node_tags;
		   page= !slide_num;struct_x=0.;struct_y=0.;
                   substructures=Array.of_list (List.rev sub)
                  }
                | _->(
                  let open Typography.OutputCommon in
                  {name="";displayname=[];metadata=[];tags=[];
		   page= -1;struct_x=0.;struct_y=0.;substructures=[||]}
                )

            in
            M.output' ~structure:(make_structure structure) (Array.map snd pages) file
          )
        in
        resolve 0 defaultEnv
    end
  end)
