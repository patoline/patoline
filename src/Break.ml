open OutputCommon
open Binary
open Constants
open CamomileLibrary
open Util
open Fonts.FTypes


(* let rec print_graph file paragraphs graph path= *)
(*   let f=open_out file in *)
(*   let rec make_path p1 p2=function *)
(*       [] | [_]->false *)
(*     | (_,h)::(a,h')::s->(p1=h && p2=h') || make_path p1 p2 ((a,h')::s) *)
(*   in *)
(*     Printf.fprintf f "digraph {\n"; *)
(*     LineMap.iter (fun k (b,_,_,a,_)-> *)
(*                     Printf.fprintf f "node_%d_%s_%s_%s [label=\"%d : %d, %d, %d\"];\n" *)
(*                       k.paragraph (if k.lineStart>=0 then string_of_int k.lineStart else "x") *)
(*                       (if k.lineEnd>=0 then string_of_int k.lineEnd else "x") *)
(*                       (if k.hyphenEnd>=0 then string_of_int k.hyphenEnd else "x") *)

(*                       k.paragraph k.lineStart k.lineEnd k.hyphenEnd; *)

(*                     Printf.fprintf f "node_%d_%s_%s_%s -> node_%d_%s_%s_%s[color=%s, label=\"\"]\n" *)
(*                       a.paragraph (if a.lineStart>=0 then string_of_int a.lineStart else "x") *)
(*                       (if a.lineEnd>=0 then string_of_int a.lineEnd else "x") *)
(*                       (if a.hyphenEnd>=0 then string_of_int a.hyphenEnd else "x") *)

(*                       k.paragraph (if k.lineStart>=0 then string_of_int k.lineStart else "x") *)
(*                       (if k.lineEnd>=0 then string_of_int k.lineEnd else "x") *)
(*                       (if k.hyphenEnd>=0 then string_of_int k.hyphenEnd else "x") *)

(*                       (if k.lastFigure<>a.lastFigure then "green" else *)
(*                          if make_path a k path then "blue" else "black") *)
(*                       (\*b k.height-a.height*\) *)
(*                  ) graph; *)
(*     Printf.fprintf f "};\n"; *)
(*     close_out f *)

(* let print_simple_graph file paragraphs graph= *)
(*   print_graph file paragraphs ( *)
(*     LineMap.fold (fun k->LineMap.add { k with height=0.; page=0 }) LineMap.empty graph *)
(*   ) [] *)


let is_last paragraph j=
  let rec is_last i=
    (i>=Array.length paragraph ||
       match paragraph.(i) with
           Glue _->is_last (i+1)
         | _->false)
  in
    is_last (j+1)

module type User=sig
  type t
  val compare:t->t->int
  val figureRef:int->t
  val figure:int->t
  val beginFigure:t->int
  val flushedFigure:t->int
  val isFigure:t->bool
  val figureNumber:t->int
end

module type Typeset=sig
  module User : User
  module UMap :
  sig
    type key = User.t
    type 'a t = 'a New_map.Make(User).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) ->
      'a t -> 'b t -> 'c t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val max_binding : 'a t -> key * 'a
    val choose : 'a t -> key * 'a
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  end
  val typeset :
    completeLine:(UMap.key Util.box array array ->
                    Util.drawingBox array ->
                      Util.line UMap.t ->
                        Util.line -> bool -> Util.line list)
    array ->
    figures:Util.drawingBox array ->
    figure_parameters:(UMap.key Util.box array array ->
                         Util.drawingBox array ->
                           Util.parameters ->
                             Util.line UMap.t ->
                               Util.line -> Util.parameters)
      array ->
    parameters:(UMap.key Util.box array array ->
                  Util.drawingBox array ->
                    Util.parameters ->
                      Util.line UMap.t ->
                        Util.line -> Util.parameters)
      array ->
    badness:(Util.line ->
               UMap.key Util.box array ->
                 int ->
                   Util.parameters ->
                     float ->
                       Util.line ->
                         UMap.key Util.box array ->
                           int -> Util.parameters -> float -> float) ->
    UMap.key Util.box array array ->
    Log.error_log list *
      (Util.parameters * Util.line) list array *
      Util.line UMap.t
end


module Make (Line:New_map.OrderedType with type t=Util.line) (User:User)=(
  struct
    module User=User
    module UMap=New_map.Make(User)
    module LineMap=New_map.Make (Line)
    module ColMap=New_map.Make (
      struct
        type t=float*float*line*float*float*line
        let compare=compare
      end)
    let haut=ref (Array.make 100 Empty)
    let max_haut=ref 0
    let bas=ref (Array.make 100 Empty)
    let max_bas=ref 0
    let writeBox arr i b=
      if i>=Array.length !arr then (
        let tmp= !arr in
          arr:=Array.make ((Array.length !arr)*2) Empty;
          for j=0 to Array.length tmp-1 do
            !arr.(j)<-tmp.(j)
          done);
    !arr.(i)<-b

  let readBox arr i= !arr.(i)

  let typeset ~completeLine ~figures ~figure_parameters ~parameters ~badness paragraphs=

    let collide line_haut params_i comp_i line_bas params_j comp_j=

      max_haut:=
        if line_haut.isFigure then
          (let fig=figures.(line_haut.lastFigure) in
             writeBox haut 0 (Drawing { fig with drawing_y1=0.; drawing_y0=fig.drawing_y0-.fig.drawing_y1 }); 1)
        else
          fold_left_line paragraphs (fun i b->writeBox haut i b; i+1) 0 line_haut;

      max_bas:=
        if line_bas.isFigure then
          (let fig=figures.(line_bas.lastFigure) in
             writeBox bas 0 (Drawing { fig with drawing_y1=0.; drawing_y0=fig.drawing_y0-.fig.drawing_y1 }); 1)
        else
          fold_left_line paragraphs (fun i b->writeBox bas i b; i+1) 0 line_bas;

      let xi=ref params_i.left_margin in
      let xj=ref params_j.left_margin in
      let rec collide i j max_col=
        let box_i=if i< !max_haut then readBox haut i else Empty in
        let box_j=if j< !max_bas then readBox bas j else Empty in
        (* let _=Graphics.wait_next_event [Graphics.Key_pressed] in *)
        let wi=box_width comp_i box_i in
        let wj=box_width comp_j box_j in
          if !xi +.wi < !xj+. wj && i < !max_haut then (
            let yi=lower_y box_i wi in
            let yj=if !xi+.wi < !xj then 0.(* -.infinity *) else
              if upper_y box_j wj > -.infinity then upper_y box_j wj else 0.
            in
              (* let x0=if !xi+.wi < !xj then !xi else max !xi !xj in *)
              (* let w0= !xi +. wi -. x0 in *)
              (* Graphics.draw_rect (round (mm*. x0)) (yj0 + round (mm*. yj)) *)
              (*   (round (mm*. (w0))) (yi0 -yj0 + round (mm*. (yi-.yj))); *)
              xi:= !xi+.wi;
              collide (i+1) j (min max_col (yi-.yj))
          ) else if j < !max_bas then (
            let yi=if !xj +. wj < !xi then 0.(* infinity *) else
              if lower_y box_i wi < infinity then lower_y box_i wi else 0. in

            let yj=upper_y box_j wj in
              (* let x0=if !xj+.wj < !xi then !xj else max !xi !xj in *)
              (* let w0= !xj +. wj -. x0 in *)
              (* Graphics.draw_rect (round (mm*. x0)) (yj0 + round (mm*. yj)) *)
              (*   (round (mm*. w0)) (yi0 -yj0 + round (mm*. (yi-.yj))); *)
              xj:= !xj+.wj;
              collide i (j+1) (min max_col (yi-.yj))
          ) else max_col
      in
        collide 0 0 infinity
    in

    let misses=ref 0 in

    let cleanup demerits0 todo0=if !misses>=50000 then (
      misses:=0;
      let rec clean demerits todo=match todo with
          []->demerits
        | h::s->
            try
              let (_,_,_,_,ant,_) as hh=LineMap.find h demerits0 in
                clean (LineMap.add h hh demerits) (ant::s)
            with
                Not_found->clean demerits s
      in
        clean LineMap.empty (List.map fst (LineMap.bindings todo0))
    ) else
      demerits0
    in

    let colision_cache=ref ColMap.empty in
    let endNode=ref None in
    let rec break allow_impossible todo demerits=
      (* A chaque etape, todo contient le dernier morceau de chemin qu'on a construit dans demerits *)
      if LineMap.is_empty todo then demerits else (
        let node,(lastBadness,lastParameters,comp0,lastUser)=LineMap.min_binding todo in
        let todo'=ref (LineMap.remove node todo) in

        (* On commence par chercher la première vraie boite après node *)
        let demerits'=ref demerits in
        let register node nextNode badness log next_params comp=
          demerits':=cleanup !demerits' !todo';
          let reallyAdd ()=
            let nextUser=Util.fold_left_line paragraphs (fun u box->match box with
                                                             User uu->UMap.add uu nextNode u
                                                           | _->u) lastUser nextNode
            in
            let nextNextUser=if nextNode.isFigure then UMap.add (User.figure nextNode.lastFigure) nextNode nextUser
            else nextUser
            in
              todo':=LineMap.add nextNode (badness,next_params,comp,nextNextUser) !todo';
              demerits':=LineMap.add nextNode (badness,log,next_params,comp,node,nextNextUser) !demerits'
          in
            try
              let bad,_,_,_,_,_=LineMap.find nextNode !demerits' in
                if bad >= badness then reallyAdd () else incr misses
            with
                Not_found->reallyAdd ()
        in

        let place_figure ()=
          let fig=figures.(node.lastFigure+1) in
          let vspace,_=line_height paragraphs node in
          let h=ceil (abs_float vspace) in
          let fig_height=(ceil (fig.drawing_y1-.fig.drawing_y0)) in
            for h'=0 to 0 do
              if node.height+.h +. float_of_int h'+.fig_height <= lastParameters.page_height then (
                let nextNode={
                  paragraph=node.paragraph; lastFigure=node.lastFigure+1; isFigure=true;
                  hyphenStart= -1; hyphenEnd= -1;
                  height=node.height+.h+. float_of_int h';
                  lineStart= -1; lineEnd= -1; paragraph_height= -1;
                  page_line=node.page_line+1; page=node.page;
                  min_width=fig.drawing_min_width;nom_width=fig.drawing_min_width;max_width=fig.drawing_min_width }
                in
                let params=figure_parameters.(node.lastFigure+1) paragraphs figures lastParameters lastUser nextNode in
                  register node nextNode
                    (lastBadness+.badness
                       node !haut 0 lastParameters 0.
                       nextNode !bas 0 params 0.)
                    Log.Normal
                    params
                    0.
              )
            done
        in
        let flushed=
          (node.lastFigure+1 < Array.length figures) &&
            (node.paragraph >= Array.length paragraphs ||
               (UMap.fold (fun k _ l->l || User.flushedFigure k > node.lastFigure) lastUser false))
        in
          if flushed then place_figure () else (
            let i,pi=
              if node.paragraph>=Array.length paragraphs || (node.hyphenEnd<0 && node.lineEnd+1>=Array.length paragraphs.(node.paragraph)) then
                (0,min (node.paragraph+1) (Array.length paragraphs))
              else if node.hyphenEnd<0 then (node.lineEnd+1, node.paragraph) else (node.lineEnd, node.paragraph)
            in
              if pi >= Array.length paragraphs then (
                if pi>node.paragraph then
                    match !endNode with
                        Some (b,_,_) when b<lastBadness->()
                      | None
                      | Some _->endNode:=Some (lastBadness, node, lastUser)
              ) else (
                (* Y a-t-il encore des boites dans ce paragraphe ? *)
                if pi<>node.paragraph then (
                  if node.lastFigure < Array.length figures-1 then
                    if UMap.fold (fun k _ l->l || User.beginFigure k > node.lastFigure) lastUser false then
                      place_figure ();
                );

                let page0,h0=if node.height>=lastParameters.page_height then (node.page+1,0.) else (node.page, node.height) in
                let r_nextNode={
                  paragraph=pi; lastFigure=node.lastFigure; isFigure=false;
                  hyphenStart= node.hyphenEnd; hyphenEnd= (-1);
                  height = h0;
                  lineStart= i; lineEnd= i;
                  paragraph_height=if i=0 then 0 else node.paragraph_height+1;
                  page_line=if page0=node.page then node.page_line+1 else 0;
                  page=page0;
                  min_width=0.;nom_width=0.;max_width=0. }
                in

                let r_params=ref lastParameters in
                let local_opt=ref [] in
                let extreme_solutions=ref [] in
                let solutions_exist=ref false in
                let rec fix page height=
                  if height>=(!r_params).page_height then
                    fix (page+1) 0.
                  else (
                    r_nextNode.height<-height;
                    r_nextNode.page<-page;
                    r_nextNode.page_line<-if page=node.page then node.page_line+1 else 0;

                    let make_next_node nextNode=
                      r_params:=parameters.(pi) paragraphs figures lastParameters lastUser nextNode;
                      let comp1=comp paragraphs !r_params.measure pi i node.hyphenEnd nextNode.lineEnd nextNode.hyphenEnd in
                      let height'=
                        if page=node.page then (
                          let rec v_distance node0 parameters=
                            if node0.isFigure then (
                              let dist=collide node0 parameters comp0 nextNode !r_params comp1 in
                                if dist < infinity then node0.height+. (ceil (-.dist)) else (
                                  try
                                    let _,_,_,_,ant,_=LineMap.find node0 !demerits' in
                                    let _,_,params,_,_,_=LineMap.find ant !demerits' in
                                      v_distance ant params
                                  with
                                      Not_found -> node0.height
                                )
                            ) else (
                              node0.height+.
                                ceil (try
                                        ColMap.find (parameters.left_margin, parameters.measure, { node0 with page=0;height=0. },
                                                     !r_params.left_margin, !r_params.measure, { nextNode with page=0;height=0. }) !colision_cache
                                      with
                                          Not_found -> (
                                            let dist=collide node0 parameters comp0 nextNode !r_params comp1 in
                                              colision_cache := ColMap.add (parameters.left_margin, parameters.measure, {node0 with page=0;height=0.},
                                                                            !r_params.left_margin, !r_params.measure, {nextNode with page=0;height=0.}) (-.dist) !colision_cache;
                                              -.dist
                                          )
                                     )
                            )
                          in
                            v_distance node lastParameters
                        ) else (
                          ceil (snd (line_height paragraphs nextNode))
                        )
                      in
                        if height>=height'
                          && (page,height) >= (node.page + !r_params.min_page_diff,
                                               node.height +. !r_params.min_height_before)
                        then (
                          let allow_orphan=
                            page=node.page || node.paragraph_height>0 in
                          let allow_widow=
                            page=node.page || (not (is_last paragraphs.(node.paragraph) nextNode.lineEnd)) in

                            if not allow_orphan && allow_widow then (
                              if allow_impossible then (
                                let _,_,_,_,last_ant,_=LineMap.find node !demerits' in
                                let ant_bad,_,ant_par, ant_comp, ant_ant,ant_user=LineMap.find last_ant !demerits' in
                                  extreme_solutions:=(ant_ant,last_ant,ant_bad,Log.Orphan node, { ant_par with page_height=node.height },ant_comp,
                                                      ant_user)::(!extreme_solutions);
                              )
                            ) else if not allow_widow && allow_orphan then (
                              if allow_impossible then (
                                let _,_,_,_,last_ant,_=LineMap.find node !demerits' in
                                let ant_bad, ant_log, ant_par, ant_comp,ant_ant, ant_user=LineMap.find last_ant !demerits' in
                                  extreme_solutions:=(ant_ant, last_ant,ant_bad, Log.Widow last_ant, { ant_par with page_height=node.height },ant_comp,
                                                      ant_user)::(!extreme_solutions);
                                  solutions_exist:=true;
                              )
                            )
                            else if nextNode.min_width > (!r_params).measure then (
                              solutions_exist:=true;
                              let nextUser=lastUser in
                              let bad=(lastBadness+.
                                         badness node !haut !max_haut lastParameters comp0
                                         nextNode !bas !max_bas !r_params comp1) in
                                local_opt:=(node,nextNode,bad,Log.Overfull_line nextNode, !r_params,comp1,nextUser)::(!local_opt);
                                (* register node nextNode bad (!r_params) *)
                            ) else (
                              solutions_exist:=true;
                              let nextUser=lastUser in
                              let bad=(lastBadness+.
                                         badness node !haut !max_haut lastParameters comp0
                                         nextNode !bas !max_bas !r_params comp1) in
                                local_opt:=(node,nextNode,bad,Log.Normal, !r_params,comp1,nextUser)::(!local_opt);
                                (* register node nextNode bad (!r_params) *)
                            )
                        )
                    in
                      List.iter make_next_node (completeLine.(pi) paragraphs figures lastUser r_nextNode allow_impossible);
                      let next_h=lastParameters.next_acceptable_height node height in
                        if (not !solutions_exist) && page<=node.page+1 then fix page (if next_h=node.height then node.height+.1. else next_h);
                  )
                in
                  (try
                     fix node.page (lastParameters.next_acceptable_height node node.height);

                     if allow_impossible && !local_opt=[] && !extreme_solutions<>[] then (
                       List.iter (fun (node,nextNode,bad,log,params,comp,user)->
                                    let a,_,_=LineMap.split nextNode !demerits' in
                                    let b,_,_=LineMap.split nextNode !todo' in
                                      demerits':=a;
                                      todo':=b
                                 ) !extreme_solutions;
                       local_opt:= !extreme_solutions
                     );
                     if !local_opt <> [] then (
                       let l0=List.sort (fun (_,_,b0,_,_,_,_) (_,_,b1,_,_,_,_)->compare b0 b1) !local_opt in
                       let deg=List.fold_left (fun m (_,_,_,_,p,_,_)->max m p.local_optimization) 0 l0 in
                       let rec register_list i l=
                         if i>0 || deg<=0 then (
                           match l with
                               []->()
                             | (node,nextNode,bad,log,params,comp,user)::s->(
                                 register node nextNode bad log params comp;
                                 register_list (i-1) s
                               )
                         )
                       in
                         register_list deg l0
                     ) else demerits':= cleanup !demerits' !todo'
                   with
                       Not_found->()
                  )
              );
          );
          break false !todo' !demerits'
      )
    in
    let first_parameters=parameters.(0) paragraphs figures default_params UMap.empty uselessLine in
    let todo0=LineMap.singleton uselessLine (0., first_parameters, 0.,UMap.empty) in
    let last_failure=ref LineMap.empty in
    let rec really_break allow_impossible todo demerits=
      let demerits'=break allow_impossible todo demerits in
        if LineMap.cardinal demerits' = 0 then (
          try
            let _=LineMap.find uselessLine !last_failure in
              if Array.length paragraphs>0 && Array.length figures > 0 then
                Printf.printf "No solution, incomplete document. Please report\n";
              demerits';
              (* raise No_solution *)
          with
              Not_found->(
                if Array.length paragraphs>0  && Array.length figures > 0 then (
                  last_failure:=LineMap.add uselessLine first_parameters !last_failure;
                  really_break true todo0 demerits'
                ) else LineMap.empty
              )
        ) else (
          if !endNode=None then (
            let (b,(bad,_,param,comp,_,user))= LineMap.max_binding demerits' in
            try
              let _=LineMap.find b !last_failure in
                Printf.printf "No solution, incomplete document. Please report\n";
                demerits'
            with
                Not_found->(
                  last_failure:=LineMap.add b param !last_failure;
                  really_break true (LineMap.singleton b (bad,param,comp,user)) demerits'
                )
          ) else
            demerits'
        )
    in
    let demerits=really_break false todo0 LineMap.empty in
      match !endNode with
          None->assert false
        | Some (_,node0,user0)->(
            try
              let rec makeParagraphs log node result=
                try
                  let _,log_,params',_,next,_=LineMap.find node demerits in
                    makeParagraphs (match log_ with Log.Normal -> log | _->log_::log) next ((params',node)::result)
                with
                    Not_found->(log,result)
              in
              let pages=Array.create (node0.page+1) [] in
              let rec makePages=function
                  []->()
                | (params,node)::s ->(
                    pages.(node.page) <- (params, node)::pages.(node.page);
                    makePages s
                  )
              in
              let log,ln=makeParagraphs [] node0 [] in
                (* print_graph "graph" paragraphs demerits ln; *)
                (* Printf.printf "Le graphe a %d nœuds\n" (LineMap.cardinal demerits); *)
                makePages ln;
                (log, pages, user0)
            with
                Not_found -> if Array.length paragraphs=0 && Array.length figures=0 then ([],[||],UMap.empty) else (
                  Printf.printf "Incomplete document, please report\n";
                  [],[||],UMap.empty
                )
          )
  end)
