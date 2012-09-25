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

module Format=functor (D:Document.DocumentStructure)->(
  struct

    module Default=DefaultFormat.Format(D)
    (* include (Default:module type of Default with module Env_center:=Default.Env_center) *)
    include Default
    let parameters a b c d e f g=
      { (Default.parameters a b c d e f g) with page_height=infinity }


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
                  D.structure:=follow (top (Node {node with displayname=M.arg1},path0)) (List.rev (List.map fst (snd !D.structure)))
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
      let do_begin_env ()=env_stack:=(List.map fst (snd !D.structure)) :: !env_stack
      let do_end_env ()=
        let rec restate st t=match t with
            Paragraph p->Paragraph { p with
              par_states=
                if p.par_states=IntSet.empty then st
                else IntSet.inter p.par_states st
            }
          | Node n->Node { n with children=IntMap.map (restate st) n.children }
          | _->t
        in
        let states=List.fold_left (fun s x->IntSet.add x s) IntSet.empty S.arg1 in
        let slide,path=follow (top !D.structure) (List.rev (List.hd !env_stack)) in
        D.structure:=(restate states slide,path);
        env_stack:=List.tl !env_stack
    end


    let mes=(slidew/.2.)*.phi
    let defaultEnv:user environment={
      Default.defaultEnv with
        normalMeasure=mes;
        normalLeftMargin=(slidew-.mes)/.2.;
        par_indent=[]
    }


    type 'a output={
      format:float*float;
    }
    let max_iterations=ref 3
    let outputParams=
      {
        format=slidew,slideh;
      }

    let output m out_params structure defaultEnv file=
      let rec resolve i env0=
        Printf.printf "Compilation %d\n" i; flush stdout;
        let tree=(fst (top (!structure))) in
        let logs=ref [] in
        let slides=ref [] in
        let reboot=ref false in
        let rec typeset_structure tree env0=
          match tree with
              Node n when List.mem_assoc "slide" n.node_tags ->(
                let out=open_out (Printf.sprintf "slide%d" (List.length !slides)) in
                doc_graph out tree;
                close_out out;

                let rec get_max_state t=match t with
                    Paragraph p->(try IntSet.max_elt p.par_states with Not_found -> 0)
                  | Node n->IntMap.fold (fun _ a m->max m (get_max_state a)) n.children 0
                  | _->0
                in
                let max_state=get_max_state tree in

                let fixable=ref false in
                let env=n.node_env env0 in
                let env1,fig_params0,params0,compl0,badnesses0,paragraphs0,_,
                  figures0,figure_trees0=flatten env fixable tree
                in

                (* Pour chaque paragraphe, l'ensemble des états où il apparaît *)
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
                let par_pos=Array.make (Array.length paragraphs0) 0. in
                let par_current=Array.make_matrix (Array.length opts)
                  (Array.length paragraphs0) 0.
                in
                for i=0 to Array.length opts-1 do
                  List.iter (fun (_,line)->
                    if line.lineStart=0 then (
                      par_current.(i).(line.paragraph)<-line.height;
                      if line.height>=par_pos.(line.paragraph) then
                        par_pos.(line.paragraph)<-line.height
                    )
                  ) opts.(i)
                done;
                for i=0 to Array.length opts-1 do
                  opts.(i)<-List.map
                    (fun (a,b)->(a,{ b with
                      height=b.height+.par_pos.(b.paragraph)-.par_current.(i).(b.paragraph)
                    }))
                    opts.(i)
                done;
                slides:=(paragraphs0,figures0,figure_trees0,opts)::(!slides);
                n.node_post_env env0 env'
              )
            | Node n->
              n.node_post_env env0 (
                IntMap.fold (fun _ a m->typeset_structure a m) n.children (n.node_env env0)
              )
            | Paragraph p->p.par_post_env env0 (p.par_env env0)
            | FigureDef f->f.fig_post_env env0 (f.fig_env env0)
        in
        Printf.fprintf stderr "Début de l'optimisation : %f s\n" (Sys.time ());
        let env=typeset_structure tree defaultEnv in
        Printf.fprintf stderr "Fin de l'optimisation : %f s\n" (Sys.time ());

        if i < !max_iterations-1 && !reboot then (
          resolve (i+1) env
        ) else (

          List.iter (List.iter (fun x->Printf.fprintf stderr "%s\n"
            (Typography.Language.message x))
          ) (List.rev !logs);

          let draw_slide (paragraphs,figures,figure_trees,opts)=
            let etats_slides=
              Array.map (fun p->
                let page={ pageFormat=(slidew,slideh); pageContents=[] } in
                let pp=Array.of_list p in
                let w,h=page.pageFormat in
                let topMargin=0. in
                let pars=ref IntMap.empty in
                for j=0 to Array.length pp-1 do
                  let param,line=pp.(j) in
                  let y=h-.topMargin-.line.height in
                  let comp=compression paragraphs param line in

                  let cur_par=ref (try IntMap.find line.paragraph !pars with Not_found->[]) in
                  let rec draw_box x y box=
                    let lowy=y+.lower_y box in
                    let uppy=y+.upper_y box in
                    match box with
                        Kerning kbox ->(
                          let fact=(box_size kbox.kern_contents/.1000.) in
                          let w=draw_box (x+.kbox.kern_x0*.fact) (y+.kbox.kern_y0*.fact)
                            kbox.kern_contents
                          in
                          w+.kbox.advance_width*.fact
                        )
                      | Hyphen h->(
                        (Array.fold_left (fun x' box->
                          let w=draw_box (x+.x') y box in
                          x'+.w) 0. h.hyphen_normal)
                      )
                      | GlyphBox a->(
                        cur_par:=
                          (OutputCommon.Glyph { a with glyph_x=a.glyph_x+.x;glyph_y=a.glyph_y+.y })
                        :: !cur_par;
                        a.glyph_size*.Fonts.glyphWidth a.glyph/.1000.
                      )
                      | Glue g
                      | Drawing g ->(
                        let w=g.drawing_min_width+.
                          comp*.(g.drawing_max_width-.g.drawing_min_width)
                        in
                        cur_par:=(List.map (translate x y) (g.drawing_contents w)) @ !cur_par;
                        w
                      )
                      | b->box_width comp b
                  in
                  let x1=fold_left_line paragraphs (fun x b->x+.draw_box x y b) param.left_margin
                    line
                  in
                  pars:=IntMap.add line.paragraph !cur_par !pars;
                done;
                IntMap.iter (fun _ a->page.pageContents<-a@page.pageContents) !pars;
                page
              ) opts
            in
            etats_slides
          in

          let pages=Array.concat (List.map draw_slide (List.rev !slides)) in

          let module M=(val m:Driver) in
          M.output pages file
        )
      in
      resolve 0 defaultEnv
  end)
