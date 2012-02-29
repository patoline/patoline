open Drivers
open Binary
open Constants
open CamomileLibrary
open Util
open Fonts.FTypes



exception Impossible
type pages=((parameters * ((float * float * box) list)) array array)


let rec print_graph file paragraphs graph path=
  let f=open_out file in
  let rec make_path p1 p2=function
      [] | [_]->false
    | (_,h)::(a,h')::s->(p1=h && p2=h') || make_path p1 p2 ((a,h')::s)
  in


    Printf.fprintf f "digraph {\n";
    LineMap.iter (fun k (b,_,a)->
                    Printf.fprintf f "node_%d_%s_%s_%s [label=\"%d, %d, %d\"];\n"
                      k.paragraph (if k.lineStart>=0 then string_of_int k.lineStart else "x")
                      (if k.lineEnd>=0 then string_of_int k.lineEnd else "x")
                      (if k.hyphenEnd>=0 then string_of_int k.hyphenEnd else "x")

                      k.lineStart k.lineEnd k.hyphenEnd;

                    Printf.fprintf f "node_%d_%s_%s_%s -> node_%d_%s_%s_%s[color=%s, label=\"%d\"]\n"
                      a.paragraph (if a.lineStart>=0 then string_of_int a.lineStart else "x")
                      (if a.lineEnd>=0 then string_of_int a.lineEnd else "x")
                      (if a.hyphenEnd>=0 then string_of_int a.hyphenEnd else "x")

                      k.paragraph (if k.lineStart>=0 then string_of_int k.lineStart else "x")
                      (if k.lineEnd>=0 then string_of_int k.lineEnd else "x")
                      (if k.hyphenEnd>=0 then string_of_int k.hyphenEnd else "x")

                      (if k.lastFigure<>a.lastFigure then "green" else
                         if make_path a k path then "blue" else "black")
                      (k.height-a.height)
                 ) graph;
    Printf.fprintf f "};\n";
    close_out f
let print_simple_graph file paragraphs graph=
  print_graph file paragraphs (
    LineMap.fold (fun k->LineMap.add { k with height=0; page=0 }) LineMap.empty graph
  ) []


let is_last paragraph j=
  let rec is_last i=
    (i>=Array.length paragraph ||
       match paragraph.(i) with
           Glue _->is_last (i+1)
         | _->false)
  in
    is_last (j+1)

exception No_solution


let typeset ~measure ~parameters ?badness:(badness=fun _ _ _ _->0.) ?figures:(figures = [||]) paragraphs=
  let collide
      paragraph_i lineStart_i lineEnd_i hyphenStart_i hyphenEnd_i xi_0 comp_i
      paragraph_j lineStart_j lineEnd_j hyphenStart_j hyphenEnd_j xj_0 comp_j=

    let xi=ref xj_0 in
    let xj=ref xi_0 in

    let rec collide boxes_i i boxes_j j max_col=

      match boxes_i, boxes_j with
          [],[]->max_col

        | (hi,_,maxi)::si, _ when i>=maxi->
            (match si with
                 (_,i0,_)::_->collide si i0 boxes_j j max_col
               | _->collide [] (-1) boxes_j j max_col)

        | _, (hj,_,maxj)::sj when j>=maxj->
            (match sj with
                 (_,j0,_)::_->collide boxes_i i sj j0 max_col
               | _->collide boxes_i i [] (-1) max_col)

        | (hi,_,maxi)::si, (hj,_,maxj)::sj when is_hyphen hi.(i) || is_hyphen hj.(j) ->
            (match hi.(i), hj.(j) with
                 Hyphen xi, Hyphen xj ->
                   collide
                     ((xi.hyphen_normal, 0, Array.length xi.hyphen_normal)::(hi, i+1, maxi)::si) 0
                     ((xj.hyphen_normal, 0, Array.length xj.hyphen_normal)::(hj, j+1, maxj)::sj) 0
                     max_col
               | Hyphen xi, _ ->
                   collide
                     ((xi.hyphen_normal, 0, Array.length xi.hyphen_normal)::(hi, i+1, maxi)::si) 0
                     boxes_j j max_col
               | _, Hyphen xj ->
                   collide
                     boxes_i i
                     ((xj.hyphen_normal, 0, Array.length xj.hyphen_normal)::(hj, j+1, maxj)::sj) 0
                     max_col
               | _->failwith "impossible case"
            )
        | _->(
            let box_i=match boxes_i with
                []->Empty
              | (hi,_,_)::_->hi.(i)
            in
            let box_j=match boxes_j with
                []->Empty
              | (hj,_,_)::_->hj.(j)
            in
              (* let _=Graphics.wait_next_event [Graphics.Key_pressed] in *)
              let wi=box_width comp_i box_i in
              let wj=box_width comp_j box_j in
                if (!xi +.wi < !xj+. wj && boxes_i<>[]) || boxes_j=[] then (
                  let yi=lower_y box_i wi in
                  let yj=if !xi+.wi < !xj then 0. else upper_y box_j wj in
                    xi:= !xi+.wi;
                    collide boxes_i (i+1) boxes_j j (min max_col (yi-.yj))
                ) else (
                  let yi=if !xj > !xi +. wi then 0. else lower_y box_i wi in
                  let yj=upper_y box_j wj in
                    xj:= !xj+.wj;
                    collide boxes_i i boxes_j (j+1) (min max_col (yi-.yj))
                )
          )
    in
    let li0=
      (paragraphs.(paragraph_i), (if hyphenStart_i>=0 then lineStart_i+1 else lineStart_i), lineEnd_i)::
        (if hyphenEnd_i>=0 then
           (match paragraphs.(paragraph_i).(lineEnd_i) with
                Hyphen x->let hyp=fst x.hyphenated.(hyphenEnd_i) in [(hyp, 0, Array.length hyp)]
              | _->[])
         else [])
    in
    let li=
      (if hyphenStart_i>=0 then
         (match paragraphs.(paragraph_i).(lineStart_i) with
              Hyphen x->let hyp=snd x.hyphenated.(hyphenStart_i) in (hyp, 0, Array.length hyp)::li0
            | _->li0)
       else li0)
    in
    let lj0=
      (paragraphs.(paragraph_j), (if hyphenStart_j>=0 then lineStart_j+1 else lineStart_j), lineEnd_j)::
        (if hyphenEnd_j>=0 then
           (match paragraphs.(paragraph_j).(lineEnd_j) with
                Hyphen x->let hyp=fst x.hyphenated.(hyphenEnd_j) in [(hyp,0,Array.length hyp)]
              | _->[])
         else [])
    in
    let lj=
      (if hyphenStart_j>=0 then
         (match paragraphs.(paragraph_j).(lineStart_j) with
              Hyphen x->(let hyp=snd x.hyphenated.(hyphenStart_j) in (hyp,0,Array.length hyp)::lj0)
            | _->lj0)
       else lj0)
    in
      match li, lj with
          (_,i,_)::_,(_,j,_)::_->
            collide li i lj j infinity
        | _->failwith "impossible case"
  in




  let log=ref [] in

  let rec break allow_impossible todo demerits=
      (* A chaque etape, todo contient le dernier morceau de chemin qu'on a construit dans demerits *)
      if LineMap.is_empty todo then demerits else (
        let node,(lastBadness,lastParameters)=LineMap.min_binding todo in
        let todo'=ref (LineMap.remove node todo) in
          if node.paragraph >= Array.length paragraphs then break false !todo' demerits else
            (
              (* On commence par chercher la première vraie boite après node *)
              let demerits'=ref demerits in
              let register node nextNode badness next_params=
                let reallyAdd ()=
                  todo':=LineMap.add nextNode (badness,next_params) !todo';
                  demerits':=LineMap.add nextNode (badness,next_params,node) !demerits'
                in
                  try
                    let bad,_,_=LineMap.find nextNode !demerits' in
                      if bad >= badness then reallyAdd ()
                  with
                      Not_found->reallyAdd ()
              in
              let i,pi=(if node.hyphenEnd<0 && node.lineEnd+1>=Array.length paragraphs.(node.paragraph) then
                          (0,node.paragraph+1)
                        else if node.hyphenEnd<0 then (node.lineEnd+1, node.paragraph) else (node.lineEnd, node.paragraph))
              in
                (* Y a-t-il encore des boites dans ce paragraphe ? *)
                if pi<>node.paragraph then (

                  if Array.length figures - node.lastFigure > 1 then (
                    let fig=figures.(node.lastFigure+1) in
                    let vspace,_=line_height paragraphs node in
                    let h=int_of_float (ceil ((abs_float vspace +. fig.drawing_y1 -. fig.drawing_y0)/.lastParameters.lead)) in
                      for h'=0 to 0 do
                        if node.height+h+h' <= lastParameters.lines_by_page - h then
                          let nextNode={
                            paragraph=pi; lastFigure=node.lastFigure+1; isFigure=true;
                            hyphenStart= -1; hyphenEnd= -1;
                            height=node.height+h+h';
                            lineStart= -1; lineEnd= -1; paragraph_height= -1; page_height=node.page_height+1; page=node.page }
                          in
                          let params=parameters lastParameters nextNode fig.drawing_min_width fig.drawing_nominal_width fig.drawing_max_width in
                            register node nextNode
                              (lastBadness+.badness node lastParameters nextNode params) params
                      done
                  )
                );

                if pi>=Array.length paragraphs then (
                  let endNode={paragraph=pi;lastFigure=node.lastFigure;hyphenStart= -1;hyphenEnd= -1; isFigure=false;
                               height=node.height; lineStart= -1; lineEnd= -1; paragraph_height= -1;
                               page_height=node.page_height+1; page=node.page } in
                    register node endNode lastBadness lastParameters;
                ) else (
                  let page0,h0=if node.height>=lastParameters.lines_by_page-1 then (node.page+1,1) else (node.page, node.height+1) in
                  let r_nextNode={
                    paragraph=pi; lastFigure=node.lastFigure; isFigure=false;
                    hyphenStart= node.hyphenEnd; hyphenEnd= (-1);
                    height = h0;
                    lineStart= i; lineEnd= i;
                    paragraph_height=if pi=node.paragraph then node.paragraph_height+1 else 0;
                    page_height=if page0=node.page then node.page_height+1 else 0;
                    page=page0 }
                  in

                  let r_params=ref lastParameters in
                  let local_opt=ref [] in
                  let solutions_exist=ref false in
                  let rec fix page height=
                    if height>=(!r_params).lines_by_page then
                      fix (page+1) 1
                    else (
                      r_nextNode.height<-height;
                      r_nextNode.page<-page;
                      r_nextNode.page_height<-if page=node.page then node.page_height+1 else 0;
                      let measure0=measure r_nextNode in
                      let rec break_next j sum_min sum_nom sum_max=
                        let make_next_node hyphen=
                          let nextNode={ r_nextNode with lineEnd=j; hyphenEnd=hyphen } in
                            r_params:=parameters lastParameters nextNode sum_min sum_nom sum_max;
                            let comp1=comp paragraphs !r_params.measure pi i node.hyphenEnd j hyphen in
                            let height'=
                              if page=node.page then (
                                let comp0=(comp paragraphs lastParameters.measure node.paragraph node.lineStart
                                             node.hyphenStart node.lineEnd node.hyphenEnd) in
                                let v_distance=collide
                                  node.paragraph node.lineStart node.lineEnd node.hyphenStart
                                  node.hyphenEnd lastParameters.left_margin comp0
                                  pi i j node.hyphenEnd hyphen !r_params.left_margin comp1
                                in
                                let fv_incr=ceil (max 1. (-.v_distance/.(!r_params).lead)) in
                                  node.height+(int_of_float fv_incr)
                              ) else (
                                int_of_float
                                  (ceil ((snd (line_height paragraphs nextNode))/.(!r_params).lead))
                              )
                            in
                              if height>=height'
                                && (page,height) >= (node.page + !r_params.min_page_diff, node.height + !r_params.min_height_diff)
                              then (
                                let allow_orphan= (!r_params).allow_orphans
                                  || page=node.page || node.paragraph_height>0 in
                                let allow_widow= (!r_params).allow_widows ||
                                  page=node.page || (not (is_last paragraphs.(node.paragraph) j)) in
                                  if not allow_orphan && allow_widow then (
                                    log:=(Orphan node)::(!log);
                                    let _,_,last_ant=LineMap.find node demerits in
                                    let ant_bad, ant_par, ant_ant=LineMap.find last_ant demerits in
                                      demerits' := LineMap.add last_ant
                                        (ant_bad, { ant_par with lines_by_page=last_ant.height+1 }, ant_ant)
                                        (LineMap.remove node !demerits');
                                      todo' := LineMap.add last_ant
                                        (ant_bad, { ant_par with lines_by_page=last_ant.height+1 })
                                        (LineMap.remove node !todo');
                                      solutions_exist:=true;

                                  ) else if not allow_widow && allow_orphan then (
                                    log:=(Widow nextNode)::(!log);
                                    let _,_, last_ant=LineMap.find node demerits in
                                    let ant_bad, ant_par, ant_ant=LineMap.find last_ant demerits in
                                      demerits' := LineMap.add last_ant
                                        (ant_bad, { ant_par with lines_by_page=last_ant.height+1 }, ant_ant)
                                        (LineMap.remove node !demerits');
                                      todo' := LineMap.add last_ant
                                        (ant_bad, { ant_par with lines_by_page=last_ant.height+1 })
                                        (LineMap.remove node !todo');
                                      solutions_exist:=true;
                                  )
                                  else if sum_min > (!r_params).measure then (
                                    log:=(Overfull_line nextNode)::(!log);
                                    solutions_exist:=true;
                                    let bad=(lastBadness+.badness node lastParameters nextNode !r_params) in
                                      local_opt:=(node,nextNode,bad,!r_params)::(!local_opt);
                                      (* register node nextNode bad (!r_params) *)
                                  ) else (
                                    solutions_exist:=true;
                                    let bad=(lastBadness+.badness node lastParameters nextNode !r_params) in
                                      local_opt:=(node,nextNode,bad,!r_params)::(!local_opt);
                                      (* register node nextNode bad (!r_params) *)
                                  )
                              )
                        in
                          if j>=Array.length (paragraphs.(pi)) then (if sum_min<=measure0 then make_next_node (-1)) else (
                            (match paragraphs.(pi).(j) with
                                 Hyphen x->(
                                   for k=0 to Array.length x.hyphenated-1 do
                                     let a,_,b=boxes_interval (fst x.hyphenated.(k)) in
                                       if sum_min+.a <= measure0 && sum_max+.b >= measure0 then (
                                         make_next_node k
                                       )
                                   done;
                                 )
                               | _->()
                            );
                            if sum_min <= measure0 then (
                              (if sum_max >= measure0 then
                                 match paragraphs.(pi).(j) with
                                     Glue _->make_next_node (-1)
                                   | _->());
                              let a,b,c=box_interval paragraphs.(pi).(j) in
                                break_next (j+1) (sum_min+. a) (sum_nom+.b) (sum_max+. c)
                            ) else if allow_impossible then (make_next_node (-1))
                          )
                      in
                        if node.hyphenEnd>=0 then (
                          match paragraphs.(node.paragraph).(node.lineEnd) with
                              Hyphen x->let a,b,c=boxes_interval (snd x.hyphenated.(node.hyphenEnd)) in
                                break_next (node.lineEnd+1) a b c
                            | _->break_next i 0. 0. 0.
                        ) else break_next i 0. 0. 0.;
                        if (not !solutions_exist) && page<=node.page+1 then fix page (height+1);
                    )
                  in
                    (try
                       fix node.page (node.height+1);
                       if !local_opt <> [] then (
                         let l0=List.sort (fun (_,_,b0,_) (_,_,b1,_)->compare b0 b1) !local_opt in
                         let deg=List.fold_left (fun m (_,_,_,p)->max m p.local_optimization) 0 l0 in
                         let rec register_list i l=
                           if i>0 || deg<=0 then (
                             match l with
                                 []->()
                               | (node,nextNode,bad,params)::s->(
                                   register node nextNode bad params;
                                   register_list (i-1) s
                                 )
                           )
                         in
                           register_list deg l0
                       )
                     with
                         Not_found->()
                    )
                );
                break false !todo' !demerits'
            )
      )
  in
  let first_line={ paragraph=0; lineStart= -1; lineEnd= -1; hyphenStart= -1; hyphenEnd= -1; isFigure=false;
                   lastFigure=(-1); height= 0;paragraph_height= -1; page_height=0; page=0 } in
  let first_parameters=parameters default_params first_line 0. 0. 0. in

  let todo0=LineMap.singleton first_line (0., first_parameters) in
  let last_failure=ref LineMap.empty in
  let rec really_break allow_impossible todo demerits=
    let demerits'=break allow_impossible todo demerits in
      if LineMap.cardinal demerits' = 0 then (
        try
          let _=LineMap.find first_line !last_failure in
            print_graph "graph" paragraphs demerits [];
            Printf.printf "No solution, incomplete document. Please report\n";
            demerits';
            (* raise No_solution *)
        with
            Not_found->(
              last_failure:=LineMap.add first_line 0 !last_failure;
              really_break true todo0 demerits'
            )
      ) else (
        let (b,(bad,param,_))= LineMap.max_binding demerits' in
          if b.paragraph < Array.length paragraphs then (
            try
              let _=LineMap.find b !last_failure in
                print_graph "graph" paragraphs demerits [];
                print_text_line paragraphs b;
                Printf.printf "No solution, incomplete document. Please report\n";
                demerits';
                (* raise No_solution *)
            with
                Not_found->(
                  last_failure:=LineMap.add b 0 !last_failure;
                  really_break true (LineMap.singleton b (bad,param)) demerits'
                )
          ) else
            demerits'
      )
  in
  let demerits=really_break false todo0 LineMap.empty in

  let rec find_last demerits0 b0 bad0 b_params0=
    let (b,(bad,b_params,_))=LineMap.max_binding demerits0 in
      if b.paragraph=b0.paragraph && b.lastFigure=b0.lastFigure then (
        if bad<bad0 then find_last (LineMap.remove b demerits0) b bad b_params
        else find_last (LineMap.remove b demerits0) b0 bad0 b_params0
      ) else (b0,b_params0)
  in
  let (b0,(bad0,b_params0,_))=LineMap.max_binding demerits in
  let (b,b_params)=find_last demerits b0 bad0 b_params0 in


  let rec makeParagraphs params node result=
    try
      let _,params',next=LineMap.find node demerits in
        makeParagraphs params' next ((params',node)::result)
    with
        Not_found->if node.paragraph>0 then raise Impossible else result
  in

  let pages=Array.create (b.page+1) [] in

  let rec makePages=function
      []->()
    | (params,node)::s ->(
        if node.paragraph<Array.length paragraphs then
          pages.(node.page) <- (params, node)::pages.(node.page);
        makePages s
      )
  in
  let ln=(makeParagraphs b_params b []) in
    print_graph "graph" paragraphs demerits ln;
    makePages ln;
    (!log, pages)
