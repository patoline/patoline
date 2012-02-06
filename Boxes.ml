open Drivers
open Binary
open Constants
open CamomileLibrary
open Util
open FontsTypes

exception Impossible
type pages=((parameters * ((float * float * box) list)) array array)

let exor a b=(a || b) && (not a || not b)

let rec print_graph file paragraphs graph=
  let f=open_out file in
    Printf.fprintf f "digraph {\n";
    LineMap.iter (fun k (_,_,a)->
                    Printf.fprintf f "node_%d_%s_%s [label=\"%d, %d\"];\n"
                      k.paragraph (if k.lineStart>=0 then string_of_int k.lineStart else "x")
                      (if k.lineEnd>=0 then string_of_int k.lineEnd else "x")
                      k.lineStart k.lineEnd;

                    Printf.fprintf f "node_%d_%s_%s -> node_%d_%s_%s[color=%s]\n"
                      a.paragraph (if a.lineStart>=0 then string_of_int a.lineStart else "x")
                      (if a.lineEnd>=0 then string_of_int a.lineEnd else "x")
                      k.paragraph (if k.lineStart>=0 then string_of_int k.lineStart else "x")
                      (if k.lineEnd>=0 then string_of_int k.lineEnd else "x")
                      (if k.lastFigure<>a.lastFigure then "green" else "black")
                 ) graph;
    Printf.fprintf f "};\n";
    close_out f


let is_first paragraph j=
  let j0=min (Array.length paragraph) j in
  let rec is_first i=
    (i>=j0 ||
       match paragraph.(i) with
           Glue _->is_first (i+1)
         | _->false)
  in
    is_first 0
let is_last paragraph j=
  let rec is_last i=
    (i>=Array.length paragraph ||
       match paragraph.(i) with
           Glue _->is_last (i+1)
         | _->false)
  in
    is_last (j+1)



let lineBreak parameters ?figures:(figures = [||]) paragraphs=

  let compression m p i hi j hj=
    let minLine=ref 0. in
    let maxLine=ref 0. in
      if hi>=0 then (
        match paragraphs.(p).(i-1) with
            Hyphen x->let a,b=boxes_interval (snd x.hyphenated.(hi)) in
              (minLine:= !minLine+.a;
               maxLine:= !maxLine+.b)
          | _->());
      if hj>=0 then (
        match paragraphs.(p).(j) with
            Hyphen x->let a,b=boxes_interval (fst x.hyphenated.(hj)) in
              (minLine:= !minLine+.a;
               maxLine:= !maxLine+.b)
          | _->());
      for k=i to j-1 do
        let a,b=box_interval paragraphs.(p).(k) in
          minLine := !minLine+.a;
          maxLine := !maxLine+.b
      done;
      max 0. (min 1. ((m-. !minLine)/.(!maxLine-. !minLine)))
  in
  let make_line pi lineStart lineEnd hyphenStart hyphenEnd=
    let arr0=
      (try
         match paragraphs.(pi).(lineStart-1) with
             Hyphen x->fst (x.hyphenated.(hyphenStart))
           | _->[||]
       with
           _->[||])
    in
    let arr1=
      (try
         match paragraphs.(pi).(lineEnd) with
             Hyphen x->fst (x.hyphenated.(hyphenEnd))
           | _->[||]
       with
           _->[||])
    in
      Array.append (Array.append arr0 (
                      if lineStart<lineEnd then
                        Array.sub paragraphs.(pi) lineStart (lineEnd-lineStart)
                      else
                        [||]
                    )) arr1
  in

  let collide line_i comp_i xi_0 line_j comp_j xj_0=
    let xi=ref xi_0 in
    let xj=ref xj_0 in
    let rec collide boxes_i i max_i boxes_j j max_j max_col=
      if i>=max_i && j>=max_j then
        max_col
      else (

        let box_i=if i>=0 && i<max_i then boxes_i.(i) else Empty in
        let box_j=if j>=0 && j<max_j then boxes_j.(j) else Empty in

          match box_i,box_j with
              Hyphen xi, Hyphen xj ->collide
                xi.hyphen_normal 0 (Array.length xi.hyphen_normal)
                xj.hyphen_normal 0 (Array.length xj.hyphen_normal) max_col
            | Hyphen xi, _ ->collide xi.hyphen_normal 0 (Array.length xi.hyphen_normal) boxes_j j max_j max_col
            | _, Hyphen xj ->collide boxes_i i max_i xj.hyphen_normal 0 (Array.length xj.hyphen_normal) max_col
            | _->(
                (* let _=Graphics.wait_next_event [Graphics.Key_pressed] in *)
                let wi=box_width comp_i box_i in
                let wj=box_width comp_j box_j in
                  if (!xi +.wi < !xj+. wj || j>=max_j) && i<max_i then (
                    let yi=lower_y box_i wi in
                    let yj=if !xi+.wi < !xj then 0. else upper_y box_j wj in
                    let x0=if !xi+.wi < !xj then !xi else max !xi !xj in
                    let w0= !xi +. wi -. x0 in
                      if w0>=0. then (
                        (* Graphics.set_color Graphics.red; *)
                        (* Graphics.draw_rect (round (mm*.x0)) (round (20.+.mm*.yj)) (round (mm*.w0)) (round (80.+.mm*.(yi-.yj))); *)
                        (* Graphics.set_color Graphics.black; *)
                        (* let _=Graphics.wait_next_event [Graphics.Key_pressed] in *)
                      );
                      xi:= !xi+.wi;
                      collide boxes_i (i+1) max_i boxes_j j max_j (min max_col (yi-.yj))

                  ) else (
                    let yi=if !xj > !xi +. wi || i>=max_i then 0. else lower_y box_i wi in
                    let yj=upper_y box_j wj in
                    let x0=if !xj+.wj < !xi then !xj else max !xi !xj in
                    let w0= !xj +. wj -. x0 in
                      if w0>=0. then (
                        (* Graphics.set_color Graphics.green; *)
                        (* Printf.printf "%d %d %d %d\n"(round (mm*.x0)) (round (200.+.mm*.yj)) (round (mm*.w0))
                           (round (80.+.mm*.(yi-.yj))); *)
                        (* Graphics.draw_rect (round (mm*.x0)) (round (20.+.mm*.yj)) (round (mm*.w0)) (round (80.+.mm*.(yi-.yj))); *)
                        (* Graphics.set_color Graphics.black; *)
                        (* let _=Graphics.wait_next_event [Graphics.Key_pressed] in *)
                      );
                      xj:= !xj+.wj;
                      collide boxes_i i max_i boxes_j (j+1) max_j (min max_col (yi-.yj))
                  )
              )
      )
    in
      collide line_i 0 (Array.length line_i) line_j 0 (Array.length line_j) infinity
  in

  let log=ref [] in

  let rec break allow_impossible todo demerits=
    let h_badness pi i j comp=
      let bad=ref 0. in
        for k=i to j-1 do
          bad:= !bad +.
            (match paragraphs.(pi).(i) with
                 Glue x->x.glue_badness (x.glue_min_width+.(x.glue_max_width-.x.glue_min_width)*.comp)
               | _->0.
            )
        done;
        !bad
    in
    let v_badness v_space line_i comp_i line_j comp_j=
      (* Calcul de la moyenne de collision *)
      let xi=ref 0. in
      let xj=ref 0. in
      let rec mean_collide boxes_i i max_i boxes_j j max_j w_tot col col2=
        if i>=max_i || j>=max_j then
          (if w_tot<=0. then 0. else (col2 -. col*.col)/.w_tot)
        else (
          match boxes_i.(i),boxes_j.(j) with
              Hyphen hi, Hyphen hj ->mean_collide
                hi.hyphen_normal 0 (Array.length hi.hyphen_normal)
                hj.hyphen_normal 0 (Array.length hj.hyphen_normal) w_tot col col2
            | Hyphen hi, _ ->mean_collide hi.hyphen_normal 0 (Array.length hi.hyphen_normal) boxes_j j max_j w_tot col col2
            | _, Hyphen hj ->mean_collide boxes_i i max_i hj.hyphen_normal 0 (Array.length hj.hyphen_normal) w_tot col col2
            | _->(
                let wi=box_width comp_i boxes_i.(i) in
                let wj=box_width comp_j boxes_j.(j) in
                  if !xi +.wi < !xj+. wj then (
                    let x0=max !xi !xj in
                    let w0= !xi+.wi-.x0 in
                    let yi=lower_y boxes_i.(i) wi in
                    let yj=upper_y boxes_j.(j) wj in

                      if !xj>= !xi+.wi || is_glue boxes_i.(i) || is_glue boxes_j.(j) then
                        (xi:= !xi+.w0;
                         mean_collide boxes_i (i+1) max_i boxes_j j max_j w_tot col col2)
                      else
                        (let area=w0*.(v_space+.yi-.yj) in
                           xi:= !xi+.w0;
                           mean_collide boxes_i (i+1) max_i boxes_j j max_j (w_tot+.w0) (col+.area) (col+.area*.area))
                  ) else (
                    let x0=max !xi !xj in
                    let w0= !xj+.wj-.x0 in
                    let yi=lower_y boxes_i.(i) wi in
                    let yj=upper_y boxes_j.(j) wj in

                      if !xi>= !xj+.wj || is_glue boxes_i.(i) || is_glue boxes_j.(j) then
                        (xj:= !xj +. w0;
                         mean_collide boxes_i i max_i boxes_j j max_j w_tot col col2)
                      else
                        (let area=w0*.(v_space+.yi-.yj) in
                           xj:= !xj +. w0;
                           mean_collide boxes_i i max_i boxes_j j max_j (w_tot+.w0) (col+.area) (col+.area*.area))
                  )
              )
        )
      in
        mean_collide line_i 0 (Array.length line_i) line_j 0 (Array.length line_j) 0. 0. 0.
    in

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
                      if bad>badness then reallyAdd ()
                  with
                      Not_found->reallyAdd ()
              in
              let i=node.lineEnd+1 in
              let current_parameters=parameters node in
                (* Y a-t-il encore des boites dans ce paragraphe ? *)
                if i>=Array.length paragraphs.(node.paragraph) then (

                  if Array.length figures - node.lastFigure > 1 then (
                    let fig=figures.(node.lastFigure+1) in
                    let h=int_of_float (ceil ((fig.drawing_y1 -. fig.drawing_y0)/.current_parameters.lead)) in
                      if node.height<=current_parameters.lines_by_page - h then
                        let nextNode={
                          paragraph=node.paragraph+1; lastFigure=node.lastFigure+1;
                          hyphenStart= -1; hyphenEnd= -1;
                          height=node.height+h;
                          lineStart= -1; lineEnd= -1; paragraph_height= -1; page=node.page }
                        in
                          register node nextNode lastBadness current_parameters;
                  );

                  let height=if (node.height=lastParameters.lines_by_page-1 ||
                      node.paragraph+1>=Array.length paragraphs) then 0 else node.height+1;
                  in
                  let nextNode={
                    paragraph=node.paragraph+1; lastFigure=node.lastFigure;
                    hyphenStart= -1; hyphenEnd= -1;
                    height=height;
                    lineStart= -1; lineEnd= -1; paragraph_height= -1;
                    page=if height=0 && node.paragraph+1 < Array.length paragraphs then node.page+1 else node.page }
                  in

                  let badness'=lastBadness in
                    register node nextNode badness' (parameters node);
                    break false !todo' !demerits'

                ) else (

                  (* Ensuite, on cherche toutes les coupes possibles. Cas particulier : la fin du paragraphe. *)
                  let params=parameters node in
                  let rec break_next j sum_min sum_max=
                    let make_next_node hyphen=
                      let comp1=compression params.measure node.paragraph i node.hyphenEnd j hyphen in
                      let v_badness,v_incr=
                        let line0=make_line node.paragraph node.lineStart node.lineEnd node.hyphenStart node.hyphenEnd in
                        let line1=make_line node.paragraph i j node.hyphenEnd hyphen in
                        let comp0=compression lastParameters.measure node.paragraph node.lineStart node.hyphenStart
                          node.lineEnd node.hyphenEnd in
                        let v_distance= if node.height+1>=params.lines_by_page then 0. else
                          collide line0 comp0 0. line1 comp1 0.
                        in
                        let v_incr=int_of_float (ceil (max 1. (-.v_distance/.params.lead))) in
                          v_badness (float_of_int v_incr*.params.lead) line0 comp0 line1 comp1, v_incr
                      in
                        if node.height+v_incr<params.lines_by_page || v_incr=1 then (

                          let next_page=if node.height+v_incr >= params.lines_by_page then node.page+1 else node.page in
                          let allow_orphan= next_page = node.page || node.paragraph_height>1 in
                          let allow_widow= next_page=node.page || (not (is_last paragraphs.(node.paragraph) j)) in
                            if (not allow_orphan && not allow_widow) || (allow_orphan && allow_widow) || allow_impossible then (
                              let nextNode={
                                paragraph=node.paragraph; lastFigure=node.lastFigure;
                                hyphenStart= node.hyphenEnd; hyphenEnd= hyphen;
                                height=
                                  if node.height+v_incr >= params.lines_by_page then 1 else
                                    node.height+v_incr;
                                lineStart= i; lineEnd= j;
                                paragraph_height=node.paragraph_height+1;
                                page=next_page }
                              in
                              let bad=(lastBadness+. v_badness*.v_badness +.
                                         (h_badness node.paragraph i j comp1) +.
                                         (if sum_min>params.measure then 1e20 else 0.)
                                      ) in
                                (* print_text_line paragraphs nextNode; *)
                                (* Printf.printf "%d %d\n" next_page node.page; flush stdout; *)

                                if not allow_orphan && allow_widow && allow_impossible then (
                                  log:=(Orphan node)::(!log);
                                  let last_bad, last_par, last_ant=LineMap.find node demerits in
                                  let replacement={ node with height=1; page=node.page+1 } in
                                    demerits' := LineMap.add replacement (last_bad, last_par, last_ant) (LineMap.remove node !demerits');
                                    todo' := LineMap.add replacement (last_bad, last_par) (LineMap.remove node !todo');
                                )
                                else if not allow_widow && allow_orphan && allow_impossible then (
                                  log:=(Widow nextNode)::(!log);
                                  let last_bad, last_par, last_ant=LineMap.find node demerits in
                                  let replacement={ node with height=1; page=node.page+1 } in
                                    demerits' := LineMap.add replacement (last_bad, last_par, last_ant) (LineMap.remove node !demerits');
                                    todo' := LineMap.add replacement (last_bad, last_par) (LineMap.remove node !todo');
                                )
                                else if sum_min > params.measure then (
                                  log:=(Overfull_line nextNode)::(!log);
                                  register node nextNode bad params
                                )
                                else
                                  register node nextNode bad params
                            )
                        )
                    in

                      if j>=Array.length (paragraphs.(node.paragraph)) then (if sum_min<=params.measure then make_next_node (-1)) else
                        match paragraphs.(node.paragraph).(j) with
                            Hyphen x->(
                              for i=0 to Array.length x.hyphenated-1 do
                                let a,b=boxes_interval (fst x.hyphenated.(i)) in
                                  if (sum_min+.a <= params.measure || allow_impossible) && sum_max+.b >= params.measure then (
                                    make_next_node i
                                  )
                              done;
                              let a,b=boxes_interval x.hyphen_normal in
                                break_next (j+1) (sum_min+. a) (sum_max+. b)
                            )
                          | _ when sum_min <= params.measure->(
                              (if sum_max >= params.measure then
                                 match paragraphs.(node.paragraph).(j) with
                                     Glue _->make_next_node (-1)
                                   | _->());

                              let a,b=box_interval paragraphs.(node.paragraph).(j) in
                                break_next (j+1) (sum_min+. a) (sum_max+. b)
                            )
                          | _ when allow_impossible -> make_next_node (-1)
                          | _->()
                  in
                    if node.hyphenEnd>=0 then (
                      match paragraphs.(node.paragraph).(node.lineEnd) with
                          Hyphen x->let a,b=boxes_interval (snd x.hyphenated.(node.hyphenEnd)) in
                            break_next (node.lineEnd+1) a b
                        | _->break_next i 0. 0.
                    ) else break_next i 0. 0.;

                    break false !todo' !demerits'
                )
            )
      )
  in
  let first_line={ paragraph=0; lineStart= -1; lineEnd= -1; hyphenStart= -1; hyphenEnd= -1;
                   lastFigure=(-1); height= 0;paragraph_height= -1; page=0 } in
  let first_parameters=parameters first_line in

  let todo0=LineMap.singleton first_line (0., first_parameters) in
  let rec really_break allow_impossible todo demerits=
    let demerits'=break allow_impossible todo demerits in
      if LineMap.cardinal demerits' = 0 then (
        log:=Overfull_line first_line :: (!log);
        really_break true todo0 demerits'
      ) else (
        let (b,(bad,param,_))= LineMap.max_binding demerits' in
          if b.paragraph < Array.length paragraphs then (
            really_break true (LineMap.singleton b (bad, param)) demerits'
          ) else
            demerits'
      )
  in
  let demerits=really_break false todo0 LineMap.empty in
    print_graph "graph" paragraphs demerits;

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
        makeParagraphs params' next ((params,node)::result)
    with
        Not_found->if node.paragraph>0 then raise Impossible else result
  in

let makeLine parameters node y=
  let comp=compression parameters.measure node.paragraph node.lineStart node.hyphenStart node.lineEnd node.hyphenEnd in
  let rec makeLine boxes x i max_i line=
    if i>=max_i then (x,line) else

      match boxes.(i) with

          Glue g->let w=g.glue_min_width+.comp*.(g.glue_max_width-.g.glue_min_width) in
            makeLine boxes (x+.w) (i+1) max_i line

        | Kerning kbox as box -> makeLine boxes (x+.(box_width comp box)) (i+1) max_i
            ((x+.kbox.kern_x0, y+.kbox.kern_y0, kbox.kern_contents)::line)

        | Hyphen h->let (a,b)=makeLine h.hyphen_normal x 0 (Array.length h.hyphen_normal) line in
            makeLine boxes a (i+1) max_i b

        | box->makeLine boxes (x+.(box_width comp box)) (i+1) max_i ((x,y,box)::line)
  in
  let u,v=(if node.hyphenStart>=0 then match paragraphs.(node.paragraph).(node.lineStart-1) with
               Hyphen x->let _,y=x.hyphenated.(node.hyphenStart) in
                 makeLine y 0. 0 (Array.length y) []
             | _->0., []
           else 0.,[])
  in
  let u',v'=makeLine paragraphs.(node.paragraph) u node.lineStart node.lineEnd v in
    if node.hyphenEnd>=0 then match paragraphs.(node.paragraph).(node.lineEnd) with
        Hyphen x->let y,_=x.hyphenated.(node.hyphenEnd) in snd (makeLine y u' 0 (Array.length y) v')
      | _->v'
    else
      v'
in
  let rec makePages page_num last_figure p pages=match p with
      []->pages
    | (params,node)::s ->(
        let pages'=if node.page<>page_num || pages=[] then
          (Array.create params.lines_by_page (params,[]))::pages else pages in
        let first=List.hd pages' in
          if node.lineEnd > node.lineStart && node.height>=0 then
            first.(node.height) <- (params, makeLine params node (float_of_int node.height *. params.lead));
          makePages node.page node.lastFigure s pages'
      )
  in
  let ln=(makeParagraphs b_params b []) in
    (!log, Array.of_list (List.rev (makePages 0 (-1) ln [])))
