open Document
open Parameters
open Fonts.FTypes
open Util
open Fonts
open OutputCommon
open Constants
open Binary

let _=Random.self_init ()


let author is_last str =
  let mcenter a b c d e l =
    { (center a b c d e l) with
        next_acceptable_height=(fun node h->max (node.height) (h+.5.)) }
  in
  newPar (Document.C.normal) mcenter [size 6. str ]

let institute is_last str =
  let mcenter a b c d e l =
    { (center a b c d e l) with
        min_height_before=11.;
        next_acceptable_height=(fun node h->max (node.height+.10.) (h+.5.)) }
  in
  newPar (Document.C.normal) mcenter [size 4. str ]

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
               str:=(Node empty,[]):: !str;
               let params a b c d e f=
                 let p=(parameters a b c d e f) in
                 let lead=env.normalLead *. (phi-.1.) in
                   { p with
                       next_acceptable_height=(fun _ h->lead*.(1.+.ceil (h/.lead)));
                   }
               in
                 newPar ~environment:(fun x->x) C.normal params
                   (T (string_of_int !page_footnotes)::(B (fun env->env.stdGlue))::l);
                 match !str with
                     []->assert false
                   | h::s->(
                       str:=s;
                       let pages=minipage { env with
                                              normalLead=env.normalLead*.(phi-.1.);
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
let graph=ref 0


module Env_itemize = struct

  let do_begin_itemize ()=
    str := (Node empty, []):: !str

  let item ()=
    let str0,str1=match !str with []->(Node empty,[]),[] | h::s->h,s in
      str:=newChild (top str0) (Node empty)::str1;
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
    match !str with
        h0::h1::s ->
          let avec_tirets = match fst (top h0) with
              Node n->Node { n with children=IntMap.map tirets n.children }
            | x->x
          in
	    str := newChild h1 (paragraphs avec_tirets)::s
      | _->assert false
end

module Env_abstract = struct

  let do_begin_abstract ()=
    str := (Node empty, []):: !str

  let do_end_abstract () =
    match !str with
        h0::h1::s ->
	  str := up (newChild h1 (fst (change_env h0
                                         (fun x->{ x with
                                                     normalLeftMargin=(x.normalLeftMargin
                                                                       +.(x.normalMeasure-.120.)/.2.);
                                                     normalMeasure=120. })))) :: s
      |_ -> assert false

end

let theoremRef name=Document.generalRef ~refType:"theorem" "th0"
module Env_theorem=struct
  let do_begin_theorem ()=
    str := (Node empty, []):: !str

  let do_end_theorem ()=
    match !str with
        h0::h1::s ->
          let rec first_par=function
              Paragraph p->
                Paragraph { p with par_contents=
                    Env (fun env->incr_counter ~level:1 env "theorem")::
                      CFix (fun env->
                              let lvl,num=(StrMap.find "theorem" env.counters) in
                              let _,str_counter=StrMap.find "structure" env.counters in
                              let sect_num=drop (List.length str_counter - lvl) str_counter in
                                alternative Bold [T "Theorem"; B (fun env->env.stdGlue); T (String.concat "." (List.map (fun x->string_of_int (x+1)) (sect_num@num)))]
                           )::
                      B (fun env->env.stdGlue)::
                      p.par_contents
                          }
            | Node n->
                let k0,_=IntMap.min_binding n.children in
                let paragraph=IntMap.singleton k0
                  (first_par (Paragraph
                                { par_contents=[]; par_env=(fun x->x);
                                  par_post_env=(fun env1 env2 -> { env1 with names=env2.names; counters=env2.counters; user_positions=env2.user_positions });
                                  par_parameters=parameters; par_completeLine=C.normal
                                }))
                in
                  Node { n with children=IntMap.fold (fun k a b->IntMap.add (k+1) a b) n.children paragraph }
            | x -> x
          in
          let stru=match fst h0 with
              Node n->
                let a,b=IntMap.min_binding n.children in
                  Node { n with children = IntMap.add a (first_par b) n.children }
            | x->first_par x
          in
	    str := up (newChild h1 stru) :: s
      |_ -> assert false

  module Env_Proof=struct
  end
end
