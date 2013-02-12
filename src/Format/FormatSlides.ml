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
open Typography.OutputCommon
open Typography.OutputPaper
open Typography.Util

open Typography.Box
open Typography.Fonts
open Typography.Fonts.FTypes
open CamomileLibrary
open Typography.Layout
open Typography.Document

module MathFonts=DefaultFormat.MathFonts
module MathsFormat=DefaultFormat.MathsFormat

let slideh=100.
let slidew=phi*.slideh
let hoffset=10.

type state={
  subautomata:state IntMap.t;
  stateContents:(float*float) IntMap.t;
  transitions:(string StrMap.t)
}


let rounded_corners ?r ?(ne=0.) ?(nw=0.) ?(se=0.) ?(sw=0.) (x0,y0) (x1,y1)=
  let ne,nw,se,sw=match r with
      Some r->r,r,r,r
    | None -> ne,nw,se,sw
  in
  let lambda=4.*.(sqrt 2.-.1.)/.3. in
  let rect=
    [|[|x1-.se;x1-.se+.lambda*.se;x1;x1|],[|y0;y0;y0+.se-.lambda*.se;y0+.se|];
      [|x1;x1|],[|y0+.se;y1-.ne|];
      [|x1;x1;x1-.ne+.lambda*.ne;x1-.ne|],[|y1-.ne;y1-.ne+.lambda*.ne;y1;y1|];
      [|x1-.ne;x0+.nw|],[|y1;y1|];
      [|x0+.nw;x0+.nw-.nw*.lambda;x0;x0|],[|y1;y1;y1-.nw+.lambda*.nw;y1-.nw|];
      [|x0;x0|],[|y1-.nw;y0+.sw|];
      [|x0;x0;x0+.sw-.lambda*.sw;x0+.sw|],[|y0+.sw;y0+.sw-.lambda*.sw;y0;y0|];
    |]
  in
  rect

let toc_background = ref black
let toc_active = ref white
let toc_inactive = ref gray
let block_background = ref black
let block_foreground = ref white


module Format=functor (D:Document.DocumentStructure)->(
  struct

    module Default=DefaultFormat.Format(D)
    include (Default:
               (((module type of Default
                 with
                   module Output:=Default.Output)
                 with
                   module Make_theorem:=Default.Make_theorem)
                with
                  module TableOfContents:=Default.TableOfContents))


    module Env_block (M:sig val arg1:Typography.Document.content list end)=struct

      let do_begin_env ()=
        D.structure:=newChildAfter !D.structure (Node empty);
        env_stack:=(List.map fst (snd !D.structure)) :: !env_stack

      let do_end_env ()=
        let stru,path=follow (top !D.structure) (List.rev (List.hd !env_stack)) in
        let par_env env={env with new_line=Default.defaultEnv.new_line } in
        let rec map_params t=match t with
            Node n->Node {n with node_env=(fun env->par_env (n.node_env env)) }
          | Paragraph p->Paragraph { p with par_env=(fun env->par_env (p.par_env env)) }
          | _->t
        in
        (* Fabriquer un paragraphe qui va bien *)
        let stru',_=paragraph ~parameters:center [C (fun env->
          try
            let alpha=0.9 in
            let margin=env.size*.0.2 in
            let mes=env.normalMeasure*.alpha in
            let env0={env with
              normalLeftMargin=margin;
              normalMeasure=mes-.2.*.margin
            }
            in
            let minip,env1=minipage' env0 (map_params stru,[]) in
            let stru_title,_=paragraph M.arg1 in
            let minip_title,env2=minipage' {env1 with fontColor=(!block_foreground)} (stru_title,[]) in
            if minip_title.(0).drawing_y0=infinity then
              minip_title.(0)<-{ minip_title.(0) with drawing_y0=0.;drawing_y1=0. };
            if minip.(0).drawing_y0=infinity then
              minip.(0)<-{ minip.(0) with drawing_y0=0.;drawing_y1=0. };

            let tx=max
              (minip.(0).drawing_y1-.minip_title.(0).drawing_y0+.3.*.margin)
              (minip_title.(0).drawing_y1-.minip_title.(0).drawing_y0+.3.*.margin)
            in
            let tit=
              if M.arg1<>[] then
                drawing_blit minip.(0) 0.
                  tx
                  minip_title.(0)
              else
                minip.(0)
            in

            let r=1. in

            let frame=
              if M.arg1=[] then [
                Path ({default with close=true},
                      [rounded_corners ~r
                          (0.,minip.(0).drawing_y0-.margin)
                          (max mes minip.(0).drawing_nominal_width+.margin,
                           minip.(0).drawing_y1+.margin)]);
              ]
              else
                [
                Path ({default with close=true},
                      [rounded_corners ~r
                          (0.,minip.(0).drawing_y0-.margin)
                          (max mes minip.(0).drawing_nominal_width+.margin,
                           tx+.minip_title.(0).drawing_y1+.margin)]);
                Path ({default with fillColor=Some !block_background;strokingColor=None },
                      [rounded_corners ~ne:r ~nw:r
                          (0.,tx+.minip_title.(0).drawing_y0-.margin)
                          (max mes minip.(0).drawing_nominal_width+.margin,
                           tx+.minip_title.(0).drawing_y1+.margin)])
              ]
            in
            let w=max mes (minip.(0).drawing_nominal_width+.margin) in
            let margin_bottom_top=0.5*.env.size in
            let dr={tit with
              drawing_y1=tit.drawing_y1+.margin_bottom_top;
              drawing_y0=tit.drawing_y0-.margin_bottom_top;
              drawing_min_width=w;drawing_nominal_width=w;drawing_max_width=w;
              drawing_contents=(fun w->(List.map (fun a->translate margin 0. (in_order 1 a)) (tit.drawing_contents w))@frame)
            } in
            [bB (fun _->[Drawing dr]);Env (fun _->env2)]
          with
              Invalid_argument _->[]
        )] in

        (* Supprimer la structure de D.structure *)
        D.structure:=
          (match path with
              []->(stru',path)
            | (a,Node b)::s->(Node { b with children=IntMap.add a stru' b.children }, s)
            | (a,b)::s->(Node { empty with children=IntMap.singleton a stru' }, s));

        env_stack:=List.tl !env_stack

    end


    module Make_theorem=functor (Th:Theorem)->struct

      module Th_=struct
        include Th
        let display _=[]
      end
      module M=Default.Make_theorem(Th_)
      let blocktitle=
        (Env (incr_counter ~level:Th_.counterLevel Th_.counter))::
        (C (fun env->
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
          Th.display (String.concat "." (List.map (fun x->string_of_int (x+1))
                                           ((List.rev sect_num)@num)))
         )::[])

      module Block=Env_block(struct let arg1=blocktitle end)
      let do_begin_env ()=
        Env_center.do_begin_env ();
        D.structure:=newChildAfter !D.structure (Node empty);

        env_stack:=(List.map fst (snd !D.structure)) :: !env_stack;
        (* newPar D.structure Complete.normal *)
        (*    (fun a b c d e f g line-> *)
        (*      (parameters a b c d e f g line)) []; *)

        Block.do_begin_env ();
        ()
      let do_end_env ()=
        Block.do_end_env ();
        M.do_end_env ();
        Env_center.do_end_env ()

    end
    module Make_theorem' (Th:Theorem) (M:sig val arg1:content list end)=struct
      module Th'=struct
        include Th
        let display x=(display x)@M.arg1
      end
      module X=Make_theorem(Th')
    end

    module Env_definition=Make_theorem
      (struct
        let refType="definition"
        let counter="definition"
        let counterLevel=0
        let display num= [tT ("Definition "^num^"."); (tT " ")]
       end)
    module Env_definition_ (M:sig val arg1:content list end)=Make_theorem
      (struct
        let refType="definition"
        let counter="definition"
        let counterLevel=0
        let display num= [tT ("Definition "^num^"."); (tT " ")]@M.arg1
       end)

    module Env_theorem=Make_theorem
      (struct
        let refType="theorem"
        let counter="theorem"
        let counterLevel=0
        let display num= [tT ("Theorem "^num^"."); (tT " ")]
       end)
    module Env_theorem_ (M:sig val arg1:content list end)=Make_theorem
      (struct
        let refType="theorem"
        let counter="theorem"
        let counterLevel=0
        let display num= [tT ("Theorem "^num^"."); (tT " ")]@M.arg1
       end)


    module Env_remarque=Make_theorem
      (struct
        let refType="remarque"
        let counter="remarque"
        let counterLevel=0
        let display num= [tT ("Remarque "^num^"."); (tT " ")]
       end)

    module Env_remarque_ (M:sig val arg1:content list end)=Make_theorem
      (struct
        let refType="remarque"
        let counter="remarque"
        let counterLevel=0
        let display num= [tT ("Remarque "^num^"."); (tT " ")]@M.arg1
       end)

    module Env_lemma=Make_theorem
      (struct
        let refType="lemma"
        let counter="lemma"
        let counterLevel=0
        let display num= [tT ("Lemma "^num^"."); (tT " ")]
       end)
    module Env_lemma_=Make_theorem'
      (struct
        let refType="lemma"
        let counter="lemma"
        let counterLevel=0
        let display num= [tT ("Lemma "^num^"."); (tT " ")]
       end)

    module Env_proposition=Make_theorem
      (struct
        let refType="proposition"
        let counter="proposition"
        let counterLevel=0
        let display num= [tT ("Proposition "^num^"."); (tT " ")]
       end)
    module Env_proposition_=Make_theorem'
      (struct
        let refType="proposition"
        let counter="proposition"
        let counterLevel=0
        let display num= [tT ("Proposition "^num^"."); (tT " ")]
       end)

    module Env_corollary=Make_theorem
      (struct
        let refType="corollary"
        let counter="corollary"
        let counterLevel=0
        let display num= [tT ("Corollary "^num^"."); (tT " ")]
       end)
    module Env_example=Make_theorem
      (struct
        let refType="example"
        let counter="example"
        let counterLevel=0
        let display num= [tT ("Example "^num^"."); (tT " ")]
       end)
    module Env_hypothesis=Make_theorem
      (struct
        let refType="hypothesis"
        let counter="hypothesis"
        let counterLevel=0
        let display num= [tT ("Hypothesis "^num^"."); (tT " ")]
       end)
    module Env_conjecture=Make_theorem
      (struct
        let refType="conjecture"
        let counter="conjecture"
        let counterLevel=0
        let display num= [tT ("Conjecture "^num^"."); (tT " ")]
       end)
    module Env_openproblem=Make_theorem
      (struct
        let refType="conjecture"
        let counter="conjecture"
        let counterLevel=0
        let display num= [tT ("Open problem "^num^"."); (tT " ")]
       end)
    module Env_openproblem'=Make_theorem'
      (struct
        let refType="conjecture"
        let counter="conjecture"
        let counterLevel=0
        let display num= [tT ("Open problem "^num^"."); (tT " ")]
       end)


    let mes=(slidew/.2.)*.phi
    let defaultEnv:environment={
      Default.defaultEnv with
        normalMeasure=mes;
        normalLeftMargin=(slidew-.mes)/.2.;
        normalLead=Default.defaultEnv.size*.1.3;
        lead=Default.defaultEnv.size*.1.3;
        par_indent=[];
        new_line=(fun env node params nextNode nextParams layout height->
          (* min height (nextNode.height-.env.lead) *)
          height-.env.lead
        );
        new_page=
        (fun t->
          let zip=Layout.make_page (slidew,slideh) (frame_top t) in
          let x0=((fst zip).frame_x0+.1.*.slidew/.6.) in
          let y0=((fst zip).frame_y0+.1.*.slideh/.6.) in
          let x1=((fst zip).frame_x1-.1.*.slidew/.6.) in
          let y1=((fst zip).frame_y1-.1.*.slideh/.6.) in
          frame x0 y0 x1 y1 zip
        )
    }

    let titleStyle x=
      size (defaultEnv.size*.1.2) (bold x)

    (* in slides, werbatim with a smaller lead *)
    let verbEnv x =
	{ (envFamily x.fontMonoFamily x)
	with size = x.size *. x.fontMonoRatio; normalMeasure=infinity; par_indent = [];
	     lead = x.lead *. x.fontMonoRatio *. 0.75}

    let parameters env b c d e f g line=
      { (Default.parameters env b c d e f g line) with
        min_lines_before=1
        (* page_height=2.*.slideh; *)
      }

    let make_toc title tt0=
      let t0,path=tt0 in
      let rec make_toc p t=match t with
          Node n when p<=1 && List.mem_assoc "intoc" n.node_tags->(
            let t1=ref (Node {empty with
              displayname=titleStyle title;
              node_tags=("slide","")::empty.node_tags;
              node_env=(incr_counter "slide")
            },[])
            in
            TableOfContents.slides center t1 t0 1;
            let nn=try fst (IntMap.min_binding n.children)-1 with Not_found->0 in
            Node { n with children=IntMap.add nn (fst (top !t1)) n.children }
          )
        | Node n when p=0 -> Node { n with children=IntMap.map (make_toc (p+1)) n.children }
        | _->t
      in
      let t1=make_toc 0 t0 in
      follow (t1,[]) (List.rev (List.map fst path))


    module TableOfContents=struct
      let do_begin_env ()=
        let max_depth=1 in
        TableOfContents.slides center D.structure (fst (top !D.structure)) max_depth
      let do_end_env ()=()
    end

    module type Title = sig
      val arg1 : (content list)
    end

    module Env_slide=struct
      module Title=functor (M:Title)->(
        struct
          let do_begin_env ()=
            let res0, path0=(follow (top !D.structure) (List.rev (List.hd !env_stack))) in
            match res0 with
                Node node->
                  D.structure:=follow (top (Node {node with displayname=titleStyle (bold M.arg1);name=string_of_contents M.arg1},path0)) (List.rev (List.map fst (snd !D.structure)))
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
          let layouts=ref [defaultEnv.new_page (empty_frame,[])] in
          let reboot=ref false in
          let toc=ref [] in
          let rec typeset_structure path tree env0=
            (* debug_env env0; *)
            let env0=
              let labl=String.concat "_" ("_"::List.map string_of_int path) in
              { env0 with
                names=StrMap.add labl (env0.counters,"_structure",uselessLine) env0.names;
                user_positions=UserMap.add (Label labl)
                  (match !layouts with
                      []->uselessLine
                    | h::_->{ uselessLine with layout=h })
                  env0.user_positions
              }
            in
            if List.length path=1 then (
              match tree with
                  Node n when List.mem_assoc "intoc" n.node_tags->(
                    toc:=(n.name,n.displayname,path,env0)::(!toc)
                  )
                | _->()
            );
            match tree with
                Node n when List.mem_assoc "slide" n.node_tags ->(
                  (*let out=open_out (Printf.sprintf "slide%d" (List.length !slides)) in
                  doc_graph out tree;
                  close_out out;*)
                  layouts:=
                    (env0.new_page (match !layouts with
                        []->(empty_frame,[])
                      | h::_->h))::(!layouts);

                  let rec get_max_state t=match t with
                      Paragraph p->(try IntSet.max_elt p.par_states with Not_found -> 0)
                    | Node n->
                      IntMap.fold (fun _ a m->max m (get_max_state a)) n.children
                        (try IntSet.max_elt n.node_states with Not_found->0)
                    | _->0
                  in
                  let max_state=get_max_state tree in

                  let fixable=ref false in
                  let env1,fig_params0,params0,new_page0,new_line0,compl0,badnesses0,paragraphs0,_,
                    figures0,figure_trees0=flatten env0 fixable tree
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
                        | _->()
                      in
                      make_paragraphs tree;
                      let fig_params=[||] and figures=[||] in
                      let params=
                        if IntMap.is_empty !par_map then [||] else
                          Array.make (IntMap.cardinal !par_map) params0.(0)
                      and new_page=
                        if IntMap.is_empty !par_map then [||] else
                          Array.make (IntMap.cardinal !par_map) new_page0.(0)
                      and new_line=
                        if IntMap.is_empty !par_map then [||] else
                          Array.make (IntMap.cardinal !par_map) new_line0.(0)
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
                        ~new_page:new_page
                        ~new_line:new_line
                        ~badness:badnesses
                        paragraphs
                      in
                      opts.(state)<-
                        List.map (fun l->
                          { l with
                            line={ l.line with paragraph=IntMap.find l.line.paragraph !par_map }
                          }
                        ) (if Array.length opt_pages>0 then snd opt_pages.(0) else []);

                      let env2,reboot'=update_names env1 figs' user' in
                      let labl_exists=
                        let labl=String.concat "_" ("_"::List.map string_of_int path) in
                        UserMap.mem (Label labl) env2.user_positions
                      in
                      typeset_states (state+1)
                        (reboot_ || (reboot' && !fixable) || not labl_exists)
                        env2
                    )
                  in

                  let reboot',env'=typeset_states 0 !reboot env1 in
                  reboot:=reboot';
                  (* Fini *)

                  (* Position la plus basse de la première ligne de chaque paragraphe *)
                  let state_start=Array.make_matrix (max_state+1)
                    (Array.length paragraphs0) infinity
                  in
                  let lowest_start=Array.make (Array.length paragraphs0) infinity in
                  Array.iteri (fun i l->
                    List.iter (fun ll->
                      if ll.line.lineStart=0 then (
                        state_start.(i).(ll.line.paragraph)<-ll.line.height;
                        lowest_start.(ll.line.paragraph)<-min
                          ll.line.height
                          lowest_start.(ll.line.paragraph)
                      )
                    ) l
                  ) opts;

                  (* Premier positionnement de tout le monde ("aligné" en haut) *)
                  let max_frame=ref (-.infinity) in
                  let min_frame=ref infinity in
                  let max_h=ref (-.infinity) in
                  let min_h=ref infinity in
                  for i=0 to Array.length opts-1 do
                    opts.(i)<-List.map (fun ll->

                      max_frame := max !max_frame (fst ll.line.layout).frame_y1;
                      min_frame := min !min_frame (fst ll.line.layout).frame_y0;

                      let y0,y1=line_height paragraphs0 [||] ll.line in
                      max_h := max !max_h (ll.line.height+.y1);
                      min_h := min !min_h (ll.line.height+.y0);

                      { ll with
                        line={ ll.line with
                          height=ll.line.height
                          -. state_start.(i).(ll.line.paragraph)
                          +.lowest_start.(ll.line.paragraph)
                        }
                      }
                    ) opts.(i)
                  done;

                  (* Centrage collectif *)
                  for i=0 to Array.length opts-1 do
                    let place=(!max_frame-. !min_frame-.(!max_h-. !min_h))/.2. in
                    let place=match classify_float place with
                        FP_infinite | FP_nan->0. | _->max 0. place
                    in
                    opts.(i)<-List.map (fun ll->
                      { ll with
                        line={ ll.line with
                          height=ll.line.height -. place
                        }
                      }
                    ) opts.(i)
                  done;

                  (* Fin du placement vertical *)

                  slides:=(path,tree,paragraphs0,figures0,figure_trees0,env1,opts)::(!slides);
                  n.node_post_env env0 env1
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
          let env0=match tree with
              Node n->n.node_env defaultEnv
            | Paragraph n->n.par_env defaultEnv
            | _->defaultEnv
          in
          let env_final=typeset_structure [] tree env0 in
          Printf.fprintf stderr "Fin de l'optimisation : %f s\n" (Sys.time ());

          if i < !max_iterations-1 && !reboot then (
            resolve (i+1) (reset_counters env_final)
          ) else (


            (* Dessin du menu en haut de l'écran *)

            let toc=List.fold_left (fun m (n,dis,path,env)->
              let a,b=try StrMap.find "_structure" env.counters with Not_found -> -1,[0] in
              IntMap.add (List.hd b) (dis,path,env) m;
            ) IntMap.empty !toc
            in

            let y_menu=slideh-.env_final.size in


            (* Dessine la table des matières du slide où on est dans l'environnement env0 *)
            let draw_toc env0=
              (* Où est-on actuellement ? *)
              let _,b0=try StrMap.find "_structure" env0.counters with Not_found -> -1,[] in
              let boxes=IntMap.map (fun (displayname,path,env)->
                let _,b=try StrMap.find "_structure" env.counters with Not_found -> -1,[0] in
                let b0=match b0 with
                    []->[] | _::s->s
                in
                let rec prefix u v=match u,v with
                  | [],_->true
                  | hu::_,hv::_ when hu<>hv->false
                  | _::su,_::sv->prefix su sv
                  | _,[]->false
                in
                if prefix (List.rev b) (List.rev b0) then (
                  let col=(!toc_active) in
                  boxify_scoped { env with fontColor=col } displayname
                ) else (
                  let col= !toc_inactive in
                  let labl=String.concat "_" ("_"::List.map string_of_int path) in
                  boxify_scoped { env with fontColor=col }
                    (bB (fun _->[User (BeginLink labl)])::
                       displayname@
                       [bB (fun _->[User EndLink])])
                )
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
              and y1=slideh-.env0.size*.phi
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
                Path ({default with fillColor=Some !toc_background},
                      [rect])
              ]
              in
              let drawn=IntMap.fold (fun _ a m->
                cont:=(List.map (fun x->in_order 1
                  (translate m y_menu (OutputCommon.resize alpha x)))
                         (draw_boxes env_final a))@(!cont);
                let w=List.fold_left (fun w x->let _,w',_=box_interval x in w +. alpha *. w') 0. a in
                m+.inter+.w
              ) boxes (start_space inter)
              in
              !cont
            in

            let draw_slide_number env i=
              let i=try List.hd (snd (StrMap.find "slide" env.counters)) with _->0 in
              let i_fin=try List.hd (snd (StrMap.find "slide" env_final.counters)) with _->0 in
              let boxes=boxify_scoped env [tT (Printf.sprintf "%d/%d" (i+1) (i_fin+1))] in
              let w=List.fold_left (fun w x->let _,w',_=box_interval x in w+.w') 0. boxes in
              let x=draw_boxes env boxes in
              List.map (translate (slidew-.w-.2.) 2.) x
            in

            (* Dessin du slide complet *)

            let draw_slide slide_number (path,tree,paragraphs,figures,figure_trees,env,opts)=
              let states=ref [] in
              for st=0 to Array.length opts-1 do
                let page={ pageFormat=slidew,slideh; pageContents=[] } in
                page.pageContents<-draw_toc env;

                let tit=
                  draw_boxes env (boxify_scoped {env with size=0.1} (match tree with
                      Node n->n.displayname
                    | _->[]))
                in
                let (_,yy0,_,yy1)=bounding_box tit in
                let y1=slideh-.env.size*.2. in

                page.pageContents<-(List.map (translate (slidew/.8.) (slideh-.hoffset*.1.1)) tit)@page.pageContents;
                let pp=Array.of_list opts.(st) in
                let w,h=slidew,slideh in
                let crosslinks=ref [] in (* (page, link, destination) *)
                let crosslink_opened=ref false in
                let destinations=ref StrMap.empty in
                let urilinks=ref None in
                for j=0 to Array.length pp-1 do
                  let param=pp.(j).line_params
                  and line=pp.(j).line in
                  let y=line.height in

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
                        | User (BeginURILink l)->(
                          let link={ link_x0=x;link_y0=y;link_x1=x;link_y1=y;uri=l;
                                     link_order=0;
                                     dest_page=(-1);dest_x=0.;dest_y=0.;is_internal=false;
                                     link_contents=[] }
                          in
                          urilinks:=Some link;
                          page.pageContents<-Link link::page.pageContents;
                          0.
                        )
                        | User (BeginLink l)->(
                          let dest_page=
                            try
                              let line=UserMap.find (Label l) env_final.user_positions in
                              print_text_line paragraphs line;
                              Layout.page line
                            with
                                Not_found->(-1)
                          in
                          let link={ link_x0=x;link_y0=y;link_x1=x;link_y1=y;uri=l;
                                     link_order=0;
                                     dest_page=dest_page;is_internal=true;
                                     dest_x=0.;dest_y=0.;
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
                          let rec link_contents u l=match l with
                              []->[]
                            | (Link h)::s->(Link { h with
                              link_contents=List.rev u
                            })::s
                            | h::s->link_contents (h::u) s
                          in
                          page.pageContents<-link_contents [] page.pageContents;
                          (match !urilinks with
                              None->(
                                match !crosslinks with
                                    []->()
                                  | (_,h,_)::s->crosslink_opened:=false; h.link_x1<-x
                              )
                            | Some h->(
                              h.link_x1<-x;
                              urilinks:=None;
                            )
                          );
                          0.
                        )
                        | b->box_width comp b
                    in
                    let x1=fold_left_line paragraphs (fun x b->x+.draw_box x y b) param.left_margin line in
                    ()
                  )
                done;
                page.pageContents<-(draw_slide_number env slide_number)@page.pageContents;
                page.pageContents<-List.rev page.pageContents;
                states:=page:: !states
              done;
              env,Array.of_list (List.rev !states)
            in
            let pages=Array.mapi draw_slide (Array.of_list (List.rev !slides)) in
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
                | Node n when List.mem_assoc "intoc" n.node_tags->
                  let sub=IntMap.fold (fun _ a m->
                    match a with
                        Node _ when List.mem_assoc "intoc" n.node_tags->(make_structure a)::m
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
