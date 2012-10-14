open OutputCommon
open Util
open CamomileLibrary
open Box
open Line
open Fonts.FTypes


let is_last paragraph j=
  let rec is_last i=
    (i>=Array.length paragraph ||
       match paragraph.(i) with
           Glue _->is_last (i+1)
         | _->false)
  in
  is_last (j+1)


type figurePosition=Placed of line | Flushed | Begun

module type Line=sig
  type t
  val compare:t->t->int
  val hash:t->int
end

module Make (L:Line with type t=Line.line) (User:Map.OrderedType)=(
  struct
    module User=User
    module UMap=New_map.Make(User)
    module LineMap=New_map.Make (L)
    module ColMap=New_map.Make (
      struct
        type t=float*float*line*float*float*line
        let compare=compare
      end)

    module H=Weak.Make(
      struct
        type t=L.t*float*TypoLanguage.message*parameters*float*(t option)*(figurePosition IntMap.t)*L.t UMap.t
        let equal (a,_,_,_,_,_,_,_) (b,_,_,_,_,_,_,_)=(L.compare a b)==0
        let hash (a,_,_,_,_,_,_,_)=L.hash a
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

    let rec print_graph file paragraphs graph path=
      let f=open_out file in
      let rec make_path p1 p2=function
          [] | [_]->false
        | (_,h)::(a,h')::s->(p1=h && p2=h') || make_path p1 p2 ((a,h')::s)
      in
      Printf.fprintf f "digraph {\n";
      LineMap.iter (fun k (b,_,_,_,a,_,_)->
                      Printf.fprintf f "node_%d_%s_%s_%s [label=\"%d : %d, %d, %d\"];\n"
                        k.paragraph (if k.lineStart>=0 then string_of_int k.lineStart else "x")
                        (if k.lineEnd>=0 then string_of_int k.lineEnd else "x")
                        (if k.hyphenEnd>=0 then string_of_int k.hyphenEnd else "x")

                        k.paragraph k.lineStart k.lineEnd k.hyphenEnd;

                      Printf.fprintf f "node_%d_%s_%s_%s -> node_%d_%s_%s_%s[color=%s, label=\"\"]\n"
                        a.paragraph (if a.lineStart>=0 then string_of_int a.lineStart else "x")
                        (if a.lineEnd>=0 then string_of_int a.lineEnd else "x")
                        (if a.hyphenEnd>=0 then string_of_int a.hyphenEnd else "x")

                        k.paragraph (if k.lineStart>=0 then string_of_int k.lineStart else "x")
                        (if k.lineEnd>=0 then string_of_int k.lineEnd else "x")
                        (if k.hyphenEnd>=0 then string_of_int k.hyphenEnd else "x")

                        (if k.lastFigure<>a.lastFigure then "green" else
                           if make_path a k path then "blue" else "black")
                        (*b k.height-a.height*)
                   ) graph;
      Printf.fprintf f "};\n";
      close_out f

    let typeset ~completeLine ~figures ~figure_parameters ~parameters ~badness paragraphs=
      if Array.length paragraphs=0 then ([],[||],IntMap.empty,UMap.empty) else begin
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
            let yi=lower_y box_i in
            let yj=if !xi+.wi < !xj then -.infinity else
              upper_y box_j
            in
            (* let x0=if !xi+.wi < !xj then !xi else max !xi !xj in *)
            (* let w0= !xi +. wi -. x0 in *)
            (* Graphics.draw_rect (round (mm*. x0)) (yj0 + round (mm*. yj)) *)
            (*   (round (mm*. (w0))) (yi0 -yj0 + round (mm*. (yi-.yj))); *)
            xi:= !xi+.wi;
            collide (i+1) j (min max_col (yi-.yj))
          ) else if j < !max_bas then (
            let yi=if !xj +. wj < !xi then infinity else
              lower_y box_i
            in
            let yj=upper_y box_j in
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


      let colision_cache=ref ColMap.empty in
      let endNode=ref None in

      let first_parameters=parameters.(0) paragraphs figures default_params IntMap.empty UMap.empty uselessLine uselessLine in
      let first_line=(uselessLine,0.,TypoLanguage.Normal,first_parameters,0.,None,IntMap.empty,UMap.empty) in
      let last_todo_line=ref first_line in
      let demerits=H.create (Array.length paragraphs) in


      let rec break allow_impossible todo=
        (* A chaque etape, todo contient le dernier morceau de chemin qu'on a construit dans demerits *)
        if not (LineMap.is_empty todo) then (
          let _,((node,lastBadness,_,lastParameters,comp0,lastNode_opt,lastFigures,lastUser) as cur_node)=LineMap.min_binding todo in
          (* print_text_line paragraphs node; *)
          (* Printf.fprintf stderr "allow_impossible : %b\n" allow_impossible;flush stderr; *)
          let todo'=ref (LineMap.remove node todo) in
          (* On commence par chercher la première vraie boite après node *)
          let register node nextNode badness log next_params comp=
            let nextUser=fold_left_line paragraphs (fun u box->match box with
                                                        User uu->UMap.add uu nextNode u
                                                      | _->u) lastUser nextNode
            in
            let add_fig k a m=try
              match IntMap.find k m with
                  Placed _->m
                | _->IntMap.add k a m
            with
                Not_found -> IntMap.add k a m
            in
            let figures1=fold_left_line paragraphs
              (fun u box->match box with
                   FlushFigure i->add_fig i Flushed u
                 | BeginFigure i->add_fig i Begun u
                 | _->u) lastFigures nextNode
            in
            let figures2=if nextNode.isFigure then
              IntMap.add nextNode.lastFigure (Placed nextNode) figures1
            else figures1
            in
            let badness=match classify_float badness with FP_infinite | FP_nan -> 0. | _->badness in
            let a=(nextNode,badness,log,next_params,comp,node,figures2,nextUser) in
            try
              let _,bad,_,_,_,_,_,_=H.find demerits a in
              if allow_impossible || bad >= badness then (
                last_todo_line:=a;
                todo':=LineMap.add nextNode a !todo';
                H.add demerits a
              )
            with Not_found->(
              last_todo_line:=a;
              todo':=LineMap.add nextNode a !todo';
              H.add demerits a
            )
          in
          let register_endNode ()=
            match !endNode with
                Some (_,b,_,_,_,_,_,_) when b<lastBadness->()
              | None
              | Some _->endNode:=Some cur_node
          in
          let place_figure ()=
            let fig=figures.(node.lastFigure+1) in
            let vspace,_=line_height paragraphs figures node in
            if node.height-.vspace +. fig.drawing_y1 -. fig.drawing_y0 <=
              lastParameters.page_height then (
                let nextNode={
                  paragraph=if node.isFigure then node.paragraph else node.paragraph+1;
                  lastFigure=node.lastFigure+1; isFigure=true;
                  hyphenStart= -1; hyphenEnd= -1;
                  height=node.height-.vspace+.fig.drawing_y1;
                  lineStart= -1; lineEnd= -1; paragraph_height= -1;
                  page_line=node.page_line+1; page=node.page;
                  min_width=fig.drawing_min_width;
                  nom_width=fig.drawing_nominal_width;
                  max_width=fig.drawing_max_width;line_y0=fig.drawing_y0;line_y1=fig.drawing_y1 }
              in
              let params=figure_parameters.(node.lastFigure+1) paragraphs figures lastParameters lastFigures lastUser node nextNode in
              let next_h=params.next_acceptable_height node lastParameters nextNode params 0. in
              let nextNode={nextNode with height=next_h } in
              register (Some cur_node) nextNode
                (lastBadness+.badness.(node.paragraph) paragraphs figures
                   lastFigures
                   node !haut 0 lastParameters 0.
                   nextNode !bas 0 params 0.)
                TypoLanguage.Normal
                params
                0.;
            ) else if allow_impossible then (
              let nextNode={
                paragraph=if node.isFigure then node.paragraph else node.paragraph+1;
                lastFigure=node.lastFigure+1; isFigure=true;
                hyphenStart= -1; hyphenEnd= -1;
                height=fig.drawing_y1;
                lineStart= -1; lineEnd= -1; paragraph_height= -1;
                page_line=node.page_line+1; page=node.page+1;
                min_width=fig.drawing_min_width;
                nom_width=fig.drawing_nominal_width;
                max_width=fig.drawing_max_width;line_y0=fig.drawing_y0;line_y1=fig.drawing_y1 }
              in
              let params=figure_parameters.(node.lastFigure+1) paragraphs figures lastParameters lastFigures lastUser node nextNode in
              register (Some cur_node) nextNode
                (lastBadness+.badness.(node.paragraph) paragraphs figures
                   lastFigures
                   node !haut 0 lastParameters 0.
                   nextNode !bas 0 params 0.)
                TypoLanguage.Normal
                params
                0.;
              );
          in

          let i,pi=
            if node.paragraph>=Array.length paragraphs then (0,node.paragraph) else
              if (node.hyphenEnd<0 && node.lineEnd+1>=
                    Array.length paragraphs.(node.paragraph)) then
                (0,min (node.paragraph+1) (Array.length paragraphs))
              else if node.hyphenEnd<0 then (node.lineEnd+1, node.paragraph) else
                (node.lineEnd, node.paragraph)
          in
          if pi >= Array.length paragraphs then (
            if node.lastFigure+1>=Array.length figures then
              register_endNode ()
            else
              place_figure ()
          ) else (
            let flushed=
              (node.lastFigure+1 < Array.length figures) &&
                (try (match IntMap.find (node.lastFigure+1) lastFigures with
                    Flushed ->true
                  | _ ->false)
                 with
                     Not_found ->false)
            in
            if (node.lineEnd+1>=Array.length paragraphs.(node.paragraph) || node.lineEnd<=0)
              && flushed
            then place_figure () else (
              if pi<>node.paragraph then (
                let placable=
                  (node.lastFigure+1 < Array.length figures) &&
                    (IntMap.mem (node.lastFigure+1) lastFigures)
                in
                if placable then place_figure ();
              );
              let page0,h0=
                if node.page<0 then (0,0.) else
                  if lastParameters.min_page_after>0 then
                    (node.page+lastParameters.min_page_after, 0.)
                  else
                    if node.height>=lastParameters.page_height then (node.page+1,0.) else
                      (node.page, node.height)
              in
              let local_opt=ref [] in
              let extreme_solutions=ref [] in
              let min_page_before=ref 0 in
              let height_problem=ref true in
              let rec fix page height n_iter=
                (* Printf.fprintf stderr "fix : %d %f\n" page height;flush stderr; *)
                let r_nextNode={
                  paragraph=pi; lastFigure=node.lastFigure; isFigure=false;
                  hyphenStart= node.hyphenEnd; hyphenEnd= (-1);
                  height = height;
                  lineStart= i; lineEnd= i;
                  paragraph_height=if i=0 then 0 else node.paragraph_height+1;
                  page_line=if page=node.page then node.page_line+1 else 0;
                  page=page;
                  min_width=0.;nom_width=0.;max_width=0.;
                  line_y0=infinity; line_y1= -.infinity }
                in
                let r_params=ref (parameters.(pi) paragraphs figures lastParameters lastFigures lastUser node r_nextNode) in

                if !r_params.page_height < infinity || !height_problem then begin

                if height>=(!r_params).page_height
                  || page < node.page+(!r_params).min_page_before
                then
                  fix (page+1) 0. (n_iter+1)
                else (
                  let minimal_tried_height=ref infinity in
                  let make_next_node nextNode=
                    r_params:=parameters.(pi) paragraphs figures lastParameters lastFigures lastUser node nextNode;
                    r_params:=fold_left_line paragraphs (fun p x->match x with
                                                             Parameters fp->fp p
                                                           | _->p) !r_params nextNode;
                    min_page_before:=max !min_page_before !r_params.min_page_before;
                    if (n_iter>= !r_params.really_next_line) || nextNode.page>node.page then (
                      let comp1=comp paragraphs !r_params.measure pi i node.hyphenEnd nextNode.lineEnd nextNode.hyphenEnd in
                      let nextNode_width=nextNode.min_width +. comp1*.(nextNode.max_width-.nextNode.min_width) in

                      let height'=
                        if lastParameters.min_page_after=0 && page=node.page && node<>uselessLine then (
                          let rec v_distance cur_node0 parameters comp0 max_dist=
                            let node0,_,_,_,_,_,_,_=cur_node0 in
                            if node0.isFigure then (
                              let fig=figures.(node0.lastFigure) in
                              max max_dist
                                (node0.height+.(snd (line_height paragraphs figures nextNode))-.fig.drawing_y0)
                            ) else (
                              let d=
                                (node0.height+.
                                   (try
                                      ColMap.find (parameters.left_margin, parameters.measure, { node0 with page=0;height=0. },
                                                   !r_params.left_margin, !r_params.measure, { nextNode with page=0;height=0. }) !colision_cache
                                    with
                                        Not_found -> (
                                          let dist=collide node0 parameters comp0 nextNode !r_params comp1 in
                                          colision_cache := ColMap.add (parameters.left_margin, parameters.measure, {node0 with page=0;height=0.},
                                                                        !r_params.left_margin, !r_params.measure, {nextNode with page=0;height=0.})
                                            (-.dist) !colision_cache;
                                          -.dist
                                        )
                                   )
                                 +. max !r_params.min_height_before parameters.min_height_after
                                )
                              in
                              let node0_width=node0.min_width +. comp0*.(node0.max_width-.node0.min_width) in

                              (try
                                 let _,_,_,_,_,prec,_,_=cur_node0 in
                                 let (prec_line,_,_,params,comp,_,_,_) as prec_=match prec with None->raise Not_found | Some a->a in

                                 let arret=
                                   (!r_params).left_margin>=parameters.left_margin
                                   && (!r_params).left_margin+.nextNode_width<=parameters.left_margin+.node0_width
                                 in
                                 if prec_line.page=page && not arret then (v_distance prec_ params comp (max d max_dist)) else
                                   max (max d max_dist) (
                                     (node0.height
                                      +. max (snd (line_height paragraphs figures nextNode))
                                        (max !r_params.min_height_before parameters.min_height_after))
                                   )
                               with
                                   Not_found->
                                     max (max d max_dist) (
                                       (node0.height
                                        +. max (snd (line_height paragraphs figures nextNode))
                                          (max !r_params.min_height_before parameters.min_height_after))
                                     )
                              )
                            )
                          in
                          v_distance cur_node lastParameters comp0 (-.infinity)
                        ) else (
                          snd (line_height paragraphs figures nextNode)
                        )
                      in
                      let node_is_orphan=
                        page<>node.page
                        &&
                          ((node.lineStart = 0
                           && node.lineEnd < Array.length (paragraphs.(node.paragraph))
                           && node.paragraph>0) (* la premiere ligne du document n'est pas orpheline *)
                           || lastParameters.not_last_line)
                        && !r_params.min_page_before<=0
                        && not node.isFigure
                      in
                      let nextNode_is_widow=
                        page<>node.page
                        &&
                          ((nextNode.lineStart > 0
                            && nextNode.lineEnd >= Array.length (paragraphs.(nextNode.paragraph)))
                           || !r_params.not_first_line)
                        && !r_params.min_page_before<=0
                      in
                      if node_is_orphan then (
                        if allow_impossible then (
                          minimal_tried_height:=min !minimal_tried_height height';
                          try
                            let _,_,_,_,_,prec,_,_=cur_node in
                            let pr,a,b,c,d,e,f,g=match prec with None->raise Not_found | Some a->a  in
                            if node.paragraph=nextNode.paragraph || (lastParameters.not_last_line && not c.not_last_line) then (
                              extreme_solutions:=(pr,a,(TypoLanguage.Opt_error (TypoLanguage.Orphan (node, text_line paragraphs node))),
                                                  { c with min_page_after=1 },
                                                  d,e,f,g)::(!extreme_solutions)
                            ) else raise Not_found
                          with
                              Not_found->(
                                extreme_solutions:=(nextNode,lastBadness,(TypoLanguage.Opt_error (TypoLanguage.Orphan (node,text_line paragraphs nextNode))),
                                                    !r_params,comp1,Some cur_node,lastFigures,lastUser)::(!extreme_solutions)
                              )
                        )
                      ) else if nextNode_is_widow then (
                        if allow_impossible then (
                          minimal_tried_height:=min !minimal_tried_height height';
                          try
                            let _,_,_,_,_,prec,_,_=cur_node in
                            let pr,a,b,c,d,e,f,g=match prec with None->raise Not_found | Some a->a  in
                            if node.paragraph=nextNode.paragraph || (!r_params.not_first_line) && not lastParameters.not_first_line then (
                              extreme_solutions:=(pr,a,(TypoLanguage.Opt_error (TypoLanguage.Widow (nextNode,text_line paragraphs nextNode))),
                                                  { c with min_page_after=1 },
                                                  d,e,f,g)::(!extreme_solutions)
                            ) else raise Not_found
                          with
                              Not_found->(
                                extreme_solutions:=(nextNode,lastBadness,(TypoLanguage.Opt_error (TypoLanguage.Widow (nextNode,text_line paragraphs nextNode))),
                                                    !r_params,comp1,Some cur_node,lastFigures,lastUser)::(!extreme_solutions)
                              )
                        )
                      )
                        else if nextNode.min_width > (!r_params).measure && allow_impossible then (
                          minimal_tried_height:=min !minimal_tried_height height';
                          if (height>=height')  then (
                            let bad=(lastBadness+.
                                       badness.(nextNode.paragraph) paragraphs figures lastFigures node !haut !max_haut lastParameters comp0
                                     nextNode !bas !max_bas !r_params comp1) in
                            local_opt:=(nextNode,
                                        max 0. bad,
                                        (TypoLanguage.Opt_error (TypoLanguage.Overfull_line (nextNode,text_line paragraphs nextNode))),
                                        !r_params,comp1,Some cur_node,lastFigures,lastUser)::(!local_opt)
                          )
                        ) else (
                          minimal_tried_height:=min !minimal_tried_height height';
                          if (height>=height') then (
                            let bad=(lastBadness+.
                                       badness.(nextNode.paragraph) paragraphs figures
                                       lastFigures node !haut !max_haut lastParameters comp0
                                       nextNode !bas !max_bas !r_params comp1) in
                            if bad<infinity || allow_impossible then
                              local_opt:=(nextNode,
                                          max 0. bad,TypoLanguage.Normal,
                                          !r_params,comp1,Some cur_node,lastFigures,lastUser)::(!local_opt)
                          ) else (
                            height_problem:=true
                          )
                        )
                    )
                  in
                  let compl=completeLine.(pi) paragraphs figures lastFigures lastUser r_nextNode allow_impossible in
                  let compl=if compl=[] && allow_impossible then (
                    [{
                      paragraph=pi; lastFigure=node.lastFigure; isFigure=false;
                      hyphenStart= node.hyphenEnd; hyphenEnd= (-1);
                      height = height;
                      lineStart= i; lineEnd= i+1;
                      paragraph_height=if i=0 then 0 else node.paragraph_height+1;
                      page_line=if page=node.page then node.page_line+1 else 0;
                      page=page;
                      min_width=0.;nom_width=0.;max_width=0.;
                      line_y0=infinity; line_y1= -.infinity }
                    ]
                  ) else compl
                  in
                  List.iter make_next_node (compl);
                  if !local_opt=[] && !extreme_solutions=[] && page<=node.page+1+(max lastParameters.min_page_after !min_page_before) then (
                    let next_h=(!r_params).next_acceptable_height node lastParameters r_nextNode !r_params !minimal_tried_height in
                    (* Printf.fprintf stderr "%f %f\n" next_h node.height; *)
                    fix page (if next_h<infinity && next_h>height then next_h else
                        height+.1.) (n_iter+1)
                  )
                )
                end
              in
              (fix page0 h0 0;
               if allow_impossible && !local_opt=[] && !extreme_solutions<>[] then (
                 List.iter (fun (nextNode,bad,log,params,comp,node,figures,user)->
                              let b,_,_=LineMap.split nextNode !todo' in
                              todo':=b
                           ) !extreme_solutions;
                 local_opt:= !extreme_solutions
               );
               if !local_opt <> [] then (
                 let l0=List.sort (fun (_,b0,_,_,_,_,_,_) (_,b1,_,_,_,_,_,_)->compare b0 b1) !local_opt in
                 let deg=List.fold_left (fun m (_,_,_,p,_,_,_,_)->max m p.local_optimization) 0 l0 in
                 let rec register_list i l=
                   if i>0 || deg<=0 then (
                     match l with
                         []->()
                       | (nextNode,bad,log,params,comp,node,fig,user)::s->(
                           register node nextNode bad log params comp;
                           register_list (i-1) s
                         )
                   )
                 in
                 register_list deg l0
               )
              )
            );
          );
          break false !todo'
        )
      in
      let last_failure=ref LineMap.empty in

      let todo0=LineMap.singleton uselessLine first_line in
      let r_todo=ref todo0 in
      let finished=ref false in
      let allow_impossible=ref false in
      while not !finished do
        break !allow_impossible !r_todo;
        if !endNode=None then (
          let (b,bad,_,param,comp,node,fig,user)= !last_todo_line in
          try
            let param0=LineMap.find b !last_failure in
            if param0.min_page_after<>param.min_page_after then raise Not_found;
            Printf.fprintf stderr "%s\n" (
              TypoLanguage.message (TypoLanguage.No_solution (text_line paragraphs b)));
            finished:=true
          with
              Not_found->(
                last_failure:=LineMap.add b param !last_failure;
                r_todo:=LineMap.singleton b !last_todo_line;
                allow_impossible:=true;
              )
        ) else finished:=true
      done;

      let (n0,_,_,_,_,_,figs0,user0) as node0=
        match !endNode with
            None-> !last_todo_line
          | Some x->x
      in
      try
        let rec makeParagraphs log node result=
          let n0,_,log_,params',_,next,_,_=node in
          match next with
              None->(log,result)
            | Some n->(
                (* print_text_line paragraphs n0; *)
                makeParagraphs (match log_ with TypoLanguage.Normal -> log | _->log_::log) n ((params',n0)::result)
              )
        in
        let pages=Array.create (n0.page+1) [] in
        let rec makePages=function
            []->()
          | (params,node)::s ->(
              pages.(node.page) <- (params, node)::pages.(node.page);
              makePages s
            )
        in
        let log,ln=makeParagraphs [] node0 [] in
        (* print_graph "graph_opt" paragraphs demerits ln; *)
        makePages ln;
        (log, Array.map (List.rev) pages, figs0,user0)
      with
          Not_found -> if Array.length paragraphs=0 && Array.length figures=0 then ([],[||],IntMap.empty,UMap.empty) else (
            Printf.fprintf stderr "%s" (TypoLanguage.message (TypoLanguage.No_solution ""));
            [],[||],IntMap.empty,UMap.empty
          )
      end
  end)
