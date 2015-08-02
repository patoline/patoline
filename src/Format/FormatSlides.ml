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
open Raw
open Color
open Util
open UsualMake
open Driver

open Typography.Box
open Fonts
open FTypes
open Typography.Document
open DefaultFormat
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
let block_title_foreground = ref white

type numbering_kind = SimpleNumbering
                    | RelativeNumbering
                    | CustomNumbering of (int -> int -> string)

let slide_numbering = ref (Some SimpleNumbering)

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
	    env_accessed := true;
            let alpha=0.9 in
            let margin=env.size*.0.2 in
            let mes=env.normalMeasure*.alpha in
            let env0={env with
              normalLeftMargin=margin;
              normalMeasure=mes-.2.*.margin
            }
            in
            let minip,env1,ms1=OutputDrawing.minipage' env0 (map_params stru,[]) in
            let stru_title,_=paragraph M.arg1 in
            let minip_title,env2,ms2=OutputDrawing.minipage' {env1 with fontColor=(!block_title_foreground)} (stru_title,[]) in
            let minip_title0=try snd (IntMap.min_binding minip_title) with Not_found->empty_drawing_box in
            let minip_title0=
              if minip_title0.drawing_y0=infinity then
                { minip_title0 with drawing_y0=0.;drawing_y1=0. }
              else minip_title0
            in
            let minip0=try snd (IntMap.min_binding minip) with Not_found->empty_drawing_box in
            let minip0=if minip0.drawing_y0=infinity then
              { minip0 with drawing_y0=0.;drawing_y1=0. }
              else minip0
            in

            let tx=max
              (minip0.drawing_y1-.minip_title0.drawing_y0+.3.*.margin)
              (minip_title0.drawing_y1-.minip_title0.drawing_y0+.3.*.margin)
            in
            let tit=
              if M.arg1<>[] then
                drawing_blit minip0 0.
                  tx
                  minip_title0
              else
                minip0
            in

            let r=1. in

            let frame=
              if M.arg1=[] then [
                Path ({default_path_param with close=true;fillColor=Some (!block_foreground)},
                      [rounded_corners ~r
                          (0.,minip0.drawing_y0-.margin)
                          (max mes minip0.drawing_nominal_width+.margin,
                           minip0.drawing_y1+.margin)]);
              ]
              else
                [
                Path ({default_path_param with close=true;fillColor=Some (!block_foreground)},
                      [rounded_corners ~r
                          (0.,minip0.drawing_y0-.margin)
                          (max mes minip0.drawing_nominal_width+.margin,
                           tx+.minip_title0.drawing_y1+.margin)]);
                Path ({default_path_param with path_order=1;fillColor=Some !block_background;strokingColor=None },
                      [rounded_corners ~ne:r ~nw:r
                          (0.,tx+.minip_title0.drawing_y0-.margin)
                          (max mes minip0.drawing_nominal_width+.margin,
                           tx+.minip_title0.drawing_y1+.margin)])
              ]
            in
            let w=max mes (minip0.drawing_nominal_width+.margin) in
            let margin_bottom_top=0.5*.env.size in
            let dr={tit with
              drawing_y1=tit.drawing_y1+.margin_bottom_top;
              drawing_y0=tit.drawing_y0-.margin_bottom_top;
              drawing_min_width=w;drawing_nominal_width=w;drawing_max_width=w;
              drawing_contents=(fun w->(List.map (fun a->Raw.translate margin 0. (in_order 2 a)) (tit.drawing_contents w))@frame)
            } in
            [bB (fun _->Drawing dr :: ms2 @ ms1);Env (fun _->env2)]
          with
              Invalid_argument _->[]
        )] in

        (* Supprimer la structure de D.structure *)
        D.structure:=
          (match path with
           | [] -> (stru',path)
           | (a,b)::s->(Node { b with children=IntMap.add a stru' b.children }, s)
          );

        env_stack:=List.tl !env_stack

    end


    module Make_theorem=functor (Th:Theorem)->struct

      module Th_=struct
        include Th
        let display _=[]
      end
      module M=Default.Make_theorem(Th_)
      let blocktitle=
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

      let reference name=lref ~refType:Th.refType name

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

    module Env_exercice=Make_theorem
      (struct
        let refType="exercice"
        let counter="exercice"
        let counterLevel=0
        let display num= [tT ("Exercice "^num^"."); (tT " ")]
       end)

    let mes=(slidew/.2.)*.phi
    let defaultEnv:environment={
      Default.defaultEnv with
        normalMeasure=mes;
        normalLeftMargin=(slidew-.mes)/.2.;
        normalLead=Default.defaultEnv.normalLead*.1.2;
        lead=Default.defaultEnv.normalLead*.1.2;
        hyphenate=(fun _->[||]);
        par_indent=[];
        new_line=(fun env node params nextNode nextParams layout height->
          (* min height (nextNode.height-.env.lead) *)
          height-.env.lead
        );
        new_page= (* useless because redefined below depending upon the presence of a title *)
        (fun t->
          let zip=Box.make_page (slidew,slideh) (frame_top t) in
          let x0=((fst zip).frame_x0+.1.*.slidew/.6.) in
          let y0= -. slideh in          (* Un peu abusif, mais tout le contenu est censé tenir *)
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
	     lead = x.lead *. x.fontMonoRatio *. 0.75;
	     normalLead = x.normalLead *. x.fontMonoRatio *. 0.75}

    let parameters env b c d e f g line=
      { (Default.parameters env b c d e f g line) with
        min_lines_before=1;
        (* page_height=2.*.slideh; *)
      }

    let make_toc title tt0=
      let t0,path=tt0 in
      let rec make_toc p t=match t with
          Node n when List.mem_assoc "intoc" n.node_tags && p>=1 ->(
            let t1=ref (Node {empty with
              displayname=titleStyle title;
              node_tags=("slide","")::empty.node_tags;
              node_env=(incr_counter "slide")
            },[])
            in
            TableOfContents.slides center t1 (ref (t0,[])) 1;
            let nn=try fst (IntMap.min_binding n.children)-1 with Not_found->0 in
            Node { n with children=IntMap.add nn (fst (top !t1))
                (IntMap.map (make_toc (p+1)) n.children) }
          )
        | Node n when p=0 -> Node { n with children=IntMap.map (make_toc (p+1)) n.children }
        | _->t
      in
      let t1=make_toc 0 t0 in
      follow (t1,[]) (List.rev (List.map fst path))


    module MainTableOfContents=struct
      let do_begin_env ()=
        let max_depth=1 in
        TableOfContents.slides ~hidden_color:(Color.black) center D.structure D.structure max_depth
      let do_end_env ()=()
    end

    module TableOfContents=struct
      let do_begin_env ()=
        let max_depth=1 in
        TableOfContents.slides center D.structure D.structure max_depth
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
                  D.structure:=follow (top (Node {node with displayname=titleStyle M.arg1;name=string_of_contents M.arg1},path0)) (List.rev (List.map fst (snd !D.structure)))
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
                if p.par_states=[] then st
                else List.filter (fun x->List.mem x st) p.par_states
            }
          | Node n->Node { n with
            children=IntMap.map (restate
                                   (if n.node_states=[] then st
                                    else List.filter (fun x->List.mem x st) n.node_states)
            ) n.children
          }
          | _->t
        in
        let states=S.arg1 in
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
      let max_iterations=ref !Config.atmost
      let outputParams=
        {
          format=slidew,slideh;
        }

      let output out_params structure defaultEnv file=

        let rec resolve comp_i env_resolved=
          Printf.eprintf "Compilation %d\n" comp_i; flush stdout;
          let tree=structure in
          let slides=ref [] in
          let reboot=ref false in
          let toc=ref [] in
          let rec typeset_structure path tree layout env0=
            let env0=
              let labl=String.concat "_" ("_"::List.map string_of_int path) in
              let li={uselessLine with layout=env0.new_page layout} in
(*	      Printf.fprintf stderr "Adding pos %s\n" labl;*)
              { env0 with
                names=StrMap.add labl (env0.counters,"_structure",li) env0.names;
                user_positions=MarkerMap.add (Label labl) li env0.user_positions }
            in
            if List.length path=1 then (
              match tree with
                  Node n when List.mem_assoc "intoc" n.node_tags->(
                    toc:=(n.name,n.displayname,path,n.node_env env0)::(!toc)
                  )
                | _->()
            );
            match tree with
                Node n when List.mem_assoc "slide" n.node_tags ->(
                  (*let out=open_out (Printf.sprintf "slide%d" (List.length !slides)) in
                  doc_graph out tree;
                  close_out out;*)
	      let hasTitle = n.displayname <> [] in
	      let env0 =
		if hasTitle then
		  { env0 with
		    new_page=
		      (fun t->
		       let zip=Box.make_page (slidew,slideh) (frame_top t) in
		       let x0=((fst zip).frame_x0+.1.*.slidew/.6.) in
		       let y0= -. slideh in          (* Un peu abusif, mais tout le contenu est censé tenir *)
		       let x1=((fst zip).frame_x1-.1.*.slidew/.6.) in
		       let y1=((fst zip).frame_y1-.1.*.slideh/.7.) in
		       frame x0 y0 x1 y1 zip
		      )
		  } else
		  { env0 with
		    new_page=
		      (fun t->
		       let zip=Box.make_page (slidew,slideh) (frame_top t) in
		       let x0=((fst zip).frame_x0+.1.*.slidew/.6.) in
		       let y0= -. slideh in          (* Un peu abusif, mais tout le contenu est censé tenir *)
		       let x1=((fst zip).frame_x1-.1.*.slidew/.6.) in
		       let y1=((fst zip).frame_y1-.1.*.slideh/.14.) in
		       frame x0 y0 x1 y1 zip
		      )
		  }
	      in
                  let rec get_max_state t=match t with
                      Paragraph p->(List.fold_left max 0 p.par_states)
                    | Node n->
                      IntMap.fold (fun _ a m->max m (get_max_state a)) n.children
                        (List.fold_left max 0 n.node_states)
                    | _->0
                  in
                  let env1,fig_params0,params0,new_page0,new_line0,compl0,badnesses0,paragraphs0,_,
                    figures0,figure_trees0=flatten ~initial_path:path env0 tree
                  in
                  let max_state=
                    Array.fold_left (
                      Array.fold_left (fun maxst box->match box with
                          Drawing d | Glue d->
                            let st1=List.fold_left max maxst d.drawing_states in
                            max st1 maxst
                        | _->maxst
                      )
                    ) (get_max_state tree) paragraphs0
                  in

                  let opts=Array.make (max_state+1) [] in

                  (* Typesetting de tous les états *)

                  let rec typeset_states state reboot_ layout0 env=
                    if state>max_state then (reboot_,layout0,env) else (
                      let real_par=ref 0 in
                      let par_map=ref IntMap.empty in
                      let rec make_paragraphs t=match t with
                          Paragraph p when List.mem state p.par_states
                              || p.par_states=[] -> (
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
                        ~initial_line:{ uselessLine with layout=(Box.frame_top layout0) }
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
                        List.fold_left (fun cont l->
                          match l with
                              Placed_line l'->
                                (try
                                   { l' with
                                     line={ l'.line with
                                       paragraph=IntMap.find l'.line.paragraph !par_map;
                                     }
                                   }::cont
                                 with
                                     Not_found->cont)
                            | _->cont
                        )
                        []
                        (try
                           all_contents
                             (snd (IntMap.max_binding (opt_pages.frame_children)))
                         with
                             Not_found->[]);
                      let next_layout=if state = 0 then opt_pages,[] else
			  let f,l = frame_down_last (opt_pages, []) in
			  frame_top ({f with frame_tags = "not_first_state"::f.frame_tags}, l)
		      in
                      let env2,reboot'=update_names env1 figs' user' in
                      let labl_exists=
                        let labl=String.concat "_" ("_"::List.map string_of_int path) in
                        MarkerMap.mem (Label labl) env2.user_positions
                      in
                      typeset_states (state+1)
                        (reboot_ || (reboot' && !(env.fixable)) || not labl_exists)
                        next_layout
                        env2
                    )
                  in
                  let reboot',layout',env'=typeset_states 0 !reboot (Box.frame_top layout) env1 in
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
                          +. lowest_start.(ll.line.paragraph)
                        }
                      }
                    ) opts.(i)
                  done;
                  min_frame:=max (slideh/.6.) !min_frame;

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

                  slides:=(path,tree,paragraphs0,figures0,figure_trees0,env',opts,
                           try
                             fst (IntMap.max_binding (fst (frame_top layout')).frame_children)
                           with
                               Not_found->(-1)
                  )::(!slides);
                  layout',n.node_post_env env0 env'
                )
              | Node n->
                let l,e=IntMap.fold (fun k a (l,e)->
                  typeset_structure (k::path) a l e
                ) n.children
                  (layout,n.node_env env0)
                in
                l,n.node_post_env env0 e
              | Paragraph p->layout,p.par_post_env env0 (p.par_env env0)
              | FigureDef f->layout,f.fig_post_env env0 (f.fig_env env0)
          in
          Printf.fprintf stderr "Début de l'optimisation : %f s\n" (Sys.time ());
          let env0=match tree with
              Node n->n.node_env env_resolved
            | Paragraph n->n.par_env env_resolved
            | _->env_resolved
          in

          let layout_final,env_final=typeset_structure [] tree (empty_frame,[]) env0 in

          Printf.fprintf stderr "Fin de l'optimisation : %f s\n" (Sys.time ());
          if comp_i < !max_iterations-1 && !reboot then (
            resolve (comp_i+1) (reset_counters env_final)
          ) else (
            (* Dessin du menu en haut de l'écran *)

            let toc=List.fold_left (fun m (n,dis,path,env)->
              let a,b=try StrMap.find "_structure" env.counters with Not_found -> -1,[0] in
              let mm=match b with _::h::_->IntMap.add h (n,dis,path,env) m | _->m in
              mm
            ) IntMap.empty !toc
            in
            let y_menu=slideh-.env_final.size in


            (* Dessine la table des matières du slide où on est dans l'environnement env0 *)
            let draw_toc env0=
              (* Où est-on actuellement ? *)
              let _,b0=try StrMap.find "_structure" env0.counters with Not_found -> -1,[0] in
              let boxes=IntMap.map (fun (n,displayname,path,env)->
                (* Printf.fprintf stderr "draw_toc %S\n" n;flush stderr; *)
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
                (*
                Printf.fprintf stderr "b:";
                List.iter (Printf.fprintf stderr "%d ") b;
                Printf.fprintf stderr "\nb0:";
                List.iter (Printf.fprintf stderr "%d ") b0;
                Printf.fprintf stderr "\n";flush stderr;
                *)
                if prefix (List.rev (match b with []->[]|_::s->s)) (List.rev b0) then (
                  let col=(!toc_active) in
                  boxify_scoped { env_final with fontColor=col } displayname
                ) else (
                  let col= !toc_inactive in
                  let labl=String.concat "_" ("_"::List.map string_of_int path) in
                  boxify_scoped { env with fontColor=col }
                    (bB (fun _->[Marker (BeginLink (Intern labl))])::
                       displayname
                     @[bB (fun _->[Marker EndLink])])
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
                Path ({default_path_param with fillColor=Some !toc_background},
                      [rect])
              ]
              in
              let _=IntMap.fold (fun _ a m->
                cont:=(List.map (fun x->in_order 1
                  (Raw.translate m y_menu (Raw.resize alpha x)))
                         (draw_boxes env_final a))@(!cont);
                let w=List.fold_left (fun w x->let _,w',_=box_interval x in w +. alpha *. w') 0. a in
                m+.inter+.w
              ) boxes (start_space inter)
              in
              !cont
            in

            (*
type numbering_kind = SimpleNumbering | RelativeNumbering
          *)

            let draw_slide_number env i=
              match !slide_numbering with
               | None    -> []
               | Some sn -> (
                 let i=try List.hd (snd (StrMap.find "slide" env.counters)) with _->0 in
                 let i_fin=try List.hd (snd (StrMap.find "slide" env_final.counters)) with _->0 in
                 let num= match sn with
                           | SimpleNumbering   -> Printf.sprintf "%d" (i+1)
                           | RelativeNumbering -> Printf.sprintf "%d/%d" (i+1) (i_fin+1)
                           | CustomNumbering f -> f i i_fin
                 in
                 let boxes=boxify_scoped env [tT num] in
                let w=List.fold_left (fun w x->let _,w',_=box_interval x in w+.w') 0. boxes in
                let x=draw_boxes env boxes in
                List.map (fun y->Raw.translate (slidew-.w-.2.) 2. (in_order max_int y)) x)
            in

            (* Dessin du slide complet *)

            let draw_slide slide_number (path,tree,paragraphs,figures,figure_trees,env,opts,slide_num)=
              let states=ref [] in
              let destinations=ref StrMap.empty in

              for st=0 to Array.length opts-1 do
                let page = empty_page (slidew,slideh) in
                page.contents<-if IntMap.cardinal toc>0 then draw_toc env else [];

                let tit=
                  match tree with
                      Node n->(
                        let minip,_,_=OutputDrawing.minipage' ~state:st { env with size=0.1 }
                          (paragraph n.displayname)
                        in
                        try let d=snd (IntMap.min_binding minip) in
                            d.drawing_contents d.drawing_nominal_width
                        with Not_found->[]
                      )
                    | _->[]
                in
                let (x0,_,x1,_)=bounding_box tit in
                page.contents<-(List.map (Raw.translate (slidew/.2.-.(x0+.x1)/.2.) (slideh-.hoffset*.1.1)) tit)@page.contents;
                let pp=Array.of_list opts.(st) in
                let crosslinks=ref [] in (* (page, link, destination) *)
                let crosslink_opened=ref false in

                let draw_line line=
                  let param=line.line_params
                  and line=line.line in
                  let y=line.height in

                  if line.isFigure then (
                    let fig=figures.(line.lastFigure) in
	            if env.show_boxes then
                      page.contents<- Path ({default_path_param with close=true;lineWidth=0.1 },
                                                [rectangle (param.left_margin,y+.fig.drawing_y0)
                                                    (param.left_margin+.fig.drawing_nominal_width,
                                                     y+.fig.drawing_y1)]) :: page.contents;
                    page.contents<- (List.map (Raw.translate param.left_margin y)
                                           (fig.drawing_contents fig.drawing_nominal_width))
                    @ page.contents;

                  ) else if line.paragraph<Array.length paragraphs then (

                    let comp=compression paragraphs param line in
                    let rec draw_box x y box=
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
                          page.contents<-
                            (Raw.Glyph { a with glyph_x=a.glyph_x+.x;glyph_y=a.glyph_y+.y })
                          :: page.contents;
                          a.glyph_size*.Fonts.glyphWidth a.glyph/.1000.
                        )
                        | Glue g
                        | Drawing g when List.mem st g.drawing_states || g.drawing_states=[]->(
                          let w=g.drawing_min_width+.comp*.(g.drawing_max_width-.g.drawing_min_width) in
                          let cont=g.drawing_contents w in
                          let cont_states=
                            List.filter (fun x->match x with
                                States s when s.states_states<>[] &&
                                    not (List.mem st s.states_states) -> false
                              | _->true
                            ) cont
                          in
                          page.contents<-
                            (List.map (Raw.translate x y) cont_states) @ page.contents;
		          if env.show_boxes then
                            page.contents<- Path ({default_path_param with close=true;lineWidth=0.1 },
						      [rectangle (x,y+.g.drawing_y0) (x+.w,y+.g.drawing_y1)])
			    :: page.contents;
                          w
                        )
                        | Marker (BeginLink l)->(
			  let k = match l with
			      Box.Extern l -> Raw.Extern l;
			    | Box.Intern l ->(
			      try
				let line=MarkerMap.find (Label l) env_final.user_positions in
                                let y1=match classify_float line.line_y1 with
                                    FP_infinite | FP_nan->
                                      0.
                                  | _->line.line_y1
                                in
				Raw.Intern(l,Box.layout_page line,0.,y1)
			      with Not_found->
				Printf.eprintf "Label not_found %s\n%!" l;
				Raw.Intern(l,-1,0.,0.)
                            )
			    | Box.Button(drag,n,d) -> Raw.Button(drag,n,d)
			  in
                          let link={ link_x0=x;link_y0=y;link_x1=x;link_y1=y;link_kind=k;
                                     link_order=0;link_closed=false;
                                     link_contents=[] }
                          in
                          crosslinks:=(comp_i, link, l) :: !crosslinks;
                          page.contents<-Link link::page.contents;
                          crosslink_opened:=true;
                          0.
                        )
                        | Marker (Label l)->(
                          let y0,y1=line_height paragraphs figures line in
                          destinations:=StrMap.add l (comp_i,param.left_margin,y+.y0,y+.y1,line) !destinations;
                          0.
                        )
                        | Marker EndLink->(
                          let rec link_contents u l=match l with
                              []->u
                            | (Link h)::s when not h.link_closed->(
                              h.link_contents<-List.rev u;
                              let (x0,y0,x1,y1)=bounding_box u in
                              h.link_y0<-y0;
                              h.link_y1<-y1;
                              h.link_closed<-true;
                              h.link_x1<-x;
                              Link h::s
                            )
                            | h::s->link_contents (h::u) s
                          in
                          page.contents<-link_contents [] page.contents;
                          crosslinks:=(match !crosslinks with []->[] | _::s->s);
                          0.
                        )
                        | b->box_width comp b
                    in
                    let _=fold_left_line paragraphs (fun x b->x+.draw_box x y b) param.left_margin line in
                    ()
                  )
                in
                for j=0 to Array.length pp-1 do
                  draw_line pp.(j)
                done;
                let rec more_contents f=
                  List.iter (fun x->match x with
                      Placed_line l->()
                    | Raw r->(
                      page.contents<-
                        (in_state st r)@page.contents
                    )
                  ) f.frame_content;
                  IntMap.iter (fun k a->more_contents a) f.frame_children;
                in
                (try
                   more_contents (IntMap.find slide_num (fst (frame_top layout_final)).frame_children)
                 with
                     Not_found->());
                page.contents<-(draw_slide_number env slide_number)@page.contents;
                states:=page:: !states
              done;
              let env=
		StrMap.fold (fun labl dest env ->
		  let comp_i,lm,y0,y1,line = dest in
(*		  Printf.fprintf stderr "Adding pos %s\n" labl;*)
		  { env with
                    user_positions=MarkerMap.add (Label labl) line (user_positions env)})
		  !destinations env
              in
              env,Array.of_list (List.rev !states)
            in

            let slide_num=ref 0 in
            (* Position dans le dernier slide de structure vu dans le parcours *)
            let structPosition=ref (0.,0.) in
            let rec make_structure pages t=
              match t with
                | Node n when List.mem_assoc "intoc" n.node_tags->(
                  let num= !slide_num in
                  let sub=IntMap.fold (fun _ a m->(make_structure pages a)@m) n.children [] in
                  let n_name = n.name in
                  let open Driver in
                      [{name=n_name;
                        raw_name=[];metadata=[];tags=n.node_tags;
		        page=num;struct_x=fst !structPosition;struct_y=snd !structPosition;
                        children=Array.of_list (List.rev sub)
                       }]
                )
                | Node n when List.mem_assoc "slide" n.node_tags ->
                  ((*Position: tout en haut du slide *)
                    structPosition:=Driver.((snd pages.(!slide_num)).(0).size);
                    incr slide_num;
                    [])
                | Node n->
                  let _=IntMap.fold (fun _ a _->ignore (make_structure pages a)) n.children () in
                  []
                | _->[]
            in

	    let pages, structure =
	      match !Config.input_bin with
		None ->
		  let pages=Array.mapi draw_slide (Array.of_list (List.rev !slides)) in
		  let str=match make_structure pages structure with
                      h::_->h
                    | []->empty_structure
                  in
		  pages, str
	      | Some fileName ->
		let ch = open_in fileName in
		let b = input_value ch in
		if not b then failwith "Wrong bin for this format";
		let structure = Marshal.from_channel ch in
		let pages = Marshal.from_channel ch in
		close_in ch;
		Printf.fprintf stderr "File %s read.\n" fileName;
		pages, structure
	    in
            M.output' ~structure (Array.map snd pages) file
          )
        in
        resolve 0 defaultEnv
    end
  end)
