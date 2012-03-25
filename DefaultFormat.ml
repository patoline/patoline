open Typography
open Parameters
open Fonts.FTypes
open Util
open Fonts
open OutputCommon
open Constants
open Binary

let _=Random.self_init ()

let ragged_left a b c d line=
  let par=parameters a b c d line in
  { par with measure=line.nom_width }

let ragged_right a b c d line=
  let par=parameters a b c d line in
  { par with
    measure=line.nom_width;
    left_margin=par.left_margin+.par.measure-.line.nom_width }

let in_text_figure a b c d line=
  let par=parameters a b c d line in
  { par with
    measure=line.nom_width;
    left_margin=par.left_margin+.par.measure-.line.nom_width;
    next_acceptable_height=(fun node h->h+.5.) }

let title is_last str =
  let mcenter a b c d l =
    { (center a b c d l) with
        min_height_before=0.;
        next_acceptable_height=(fun node h->max (node.height+.20.) (h+.5.)) }
  in
  newPar (Typography.C.normal 150.) mcenter [size 10. str ]

let author is_last str =
  let mcenter a b c d l =
    { (center a b c d l) with
        next_acceptable_height=(fun node h->max (node.height) (h+.5.)) }
  in
  newPar (Typography.C.normal 150.) mcenter [size 6. str ]

let institute is_last str =
  let mcenter a b c d l =
    { (center a b c d l) with
        min_height_before=11.;
        next_acceptable_height=(fun node h->max (node.height+.10.) (h+.5.)) }
  in
  newPar (Typography.C.normal 150.) mcenter [size 4. str ]

let textWidth : Typography.user Typography.C.completion= Typography.C.normal 150.

let lang_OCaml s = [T s]



let minipage env str=
  let fig_params,params,compl,pars,figures=flatten env (fst str) in
    let (_,pages,user')=TS.typeset
      ~completeLine:compl
      ~figure_parameters:fig_params
      ~figures:figures
      ~parameters:params
      ~badness:(Badness.badness pars)
      pars
    in
      OutputDrawing.output pars figures env pages

let footnote l=
    [Env (fun env->
            let next=match try StrMap.find "footnotes" env.counters with Not_found -> [] with
                []->0
              | h::_->h
            in
              { env with counters=StrMap.add "footnotes" [next+1] env.counters });
     BFix (fun env->
             let count=match try StrMap.find "footnotes" env.counters with Not_found -> [] with
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
                 let params a b c d e=
                   let p=(parameters a b c d e) in
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
