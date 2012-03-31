open Typography
open Parameters
open Fonts.FTypes
open Util
open Fonts
open OutputCommon
open Constants
open Binary

let _=Random.self_init ()

let ragged_left a b c d e line=
  let par=parameters a b c d e line in
  { par with measure=line.nom_width }

let ragged_right a b c d e line=
  let par=parameters a b c d e line in
  { par with
    measure=line.nom_width;
    left_margin=par.left_margin+.par.measure-.line.nom_width }

let in_text_figure a b c d e line=
  let par=parameters a b c d e line in
  { par with
    measure=line.nom_width;
    left_margin=par.left_margin+.par.measure-.line.nom_width;
    next_acceptable_height=(fun node h->h+.5.) }

let title is_last str =
  let mcenter a b c d e l =
    { (center a b c d e l) with
        min_height_before=0.;
        next_acceptable_height=(fun node h->max (node.height+.20.) (h+.5.)) }
  in
  newPar (Typography.C.normal) mcenter [size 10. str ]

let author is_last str =
  let mcenter a b c d e l =
    { (center a b c d e l) with
        next_acceptable_height=(fun node h->max (node.height) (h+.5.)) }
  in
  newPar (Typography.C.normal) mcenter [size 6. str ]

let institute is_last str =
  let mcenter a b c d e l =
    { (center a b c d e l) with
        min_height_before=11.;
        next_acceptable_height=(fun node h->max (node.height+.10.) (h+.5.)) }
  in
  newPar (Typography.C.normal) mcenter [size 4. str ]

let textWidth= Typography.C.normal

let lang_OCaml s = [T s]



let minipage env str=
  let env',fig_params,params,compl,pars,figures=flatten env (fst str) in
    let (_,pages,user')=TS.typeset
      ~completeLine:compl
      ~figure_parameters:fig_params
      ~figures:figures
      ~parameters:params
      ~badness:(Badness.badness pars)
      pars
    in
      OutputDrawing.output pars figures env' pages

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
                                 Footnote (i,_) when i= count -> foot_num:=a.Util.page
                               | _->()
                          ) env.user_positions;
               (* Y a-t-il deja des footnotes sur cette page ? *)
               TS.UMap.iter (fun k a->
                               match k with
                                   Footnote (i,_) when a.Util.page= !foot_num && i< count -> incr page_footnotes
                                 | _->()
                            ) env.user_positions;
               (* Insertion d'une footnote *)
               let str'=ref (Node empty,[]) in
                 let params a b c d e f=
                   let p=(parameters a b c d e f) in
                   let lead=env.normalLead *. (phi-.1.) in
                     { p with
                         next_acceptable_height=(fun _ h->lead*.(1.+.ceil (h/.lead)));
                     }
                 in
                   newPar ~structure:str' ~environment:(fun x->x) textWidth params
                     (T (string_of_int !page_footnotes)::(B (fun env->env.stdGlue))::l);
                   let pages=minipage { env with
                                          normalLead=env.normalLead*.(phi-.1.);
                                          size=env.size*.(phi-.1.) }
                     (top !str') in
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
let graph=ref 0

let stack = ref []

module Env_itemize = struct

  let do_begin_itemize ()=
    stack := !str::!stack;
    str := Node empty, []

  let item ()=str:=newChild (top !str) (Node empty); []
  let addon = [ T "–"; B (fun env->[glue env.size env.size env.size]) ]


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
                          parameters=params p.parameters }
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
                    parameters=params1 p.parameters
                  }
                | t -> t) chi }
	  with Not_found -> t)
      | t->t
    in
    let avec_tirets = match fst (top !str) with
        Node n->Node { n with children=IntMap.map tirets n.children }
      | x->x
    in
    match !stack with [] -> assert false
    | st::s ->
	str := newChild st (paragraphs avec_tirets);
	stack := s
end

module Env_abstract = struct

  let do_begin_abstract ()=
    stack := !str::!stack;
    str := Node empty, []
  let do_end_abstract () =
    match !stack with
        [] -> assert false
      | st::s ->
	  str := up (newChild st (fst (change_env !str
                                         (fun x->{ x with
                                                     normalLeftMargin=(x.normalLeftMargin
                                                                       +.(x.normalMeasure-.120.)/.2.);
                                                     normalMeasure=120. }))));
	  stack := s;
end

let theoremRef name=Typography.generalRef ~refType:"theorem" "th0"
module Env_theorem=struct
  let do_begin_theorem ()=
    stack := !str::!stack;
    str := Node empty, []
  let do_end_theorem ()=
    match !stack with
        [] -> assert false
      | st::s ->
          let success=ref false in
          let rec first_par=function
              Paragraph p->
                success:=true;
                Paragraph { p with par_contents=
                    Env (fun env->incr_counter ~level:1 env "theorem")::
                      C (fun env->
                           let lvl,num=(StrMap.find "theorem" env.counters) in
                           let _,str_counter=StrMap.find "structure" env.counters in
                           let sect_num=drop (List.length str_counter - lvl) str_counter in
                             alternative Bold [T "Theorem"; B (fun env->env.stdGlue); T (String.concat "." (List.map (fun x->string_of_int (x+1)) (sect_num@num)))]
                        )::
                      B (fun env->env.stdGlue)::
                      p.par_contents
                          }
            | Node n->Node { n with children=IntMap.map (fun x->if !success then x else first_par x) n.children }
            | x -> x
          in
	    str := up (newChild st (first_par (fst !str)));
	    stack := s;
  module Env_Proof=struct
  end
end
