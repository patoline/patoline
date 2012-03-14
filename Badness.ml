open Util


let v_badness paragraphs v_space node0 x0 comp0 node1 x1 comp1=
  let xi=ref x0 in
  let xj=ref x1 in

  let rec v_badness boxes_i i boxes_j j w_tot col col2=
    match boxes_i, boxes_j with
        [],_ | _,[] ->if w_tot<=0. then 0. else ((col*.col-.col2)/.w_tot)

      | (hi,_,maxi)::si, _ when i>=maxi->
          (match si with
               (_,i0,_)::_->v_badness si i0 boxes_j j w_tot col col2
             | _->v_badness [] (-1) boxes_j j w_tot col col2)

      | _, (hj,_,maxj)::sj when j>=maxj->
          (match sj with
               (_,j0,_)::_->v_badness boxes_i i sj j0 w_tot col col2
             | _->v_badness boxes_i i [] (-1) w_tot col col2)

      | (hi,_,maxi)::si, (hj,_,maxj)::sj ->
          (match hi.(i), hj.(j) with
               Hyphen xi, Hyphen xj ->
                 v_badness
                   ((xi.hyphen_normal, 0, Array.length xi.hyphen_normal)::(hi, i+1, maxi)::si) 0
                   ((xj.hyphen_normal, 0, Array.length xj.hyphen_normal)::(hj, j+1, maxj)::sj) 0
                   w_tot col col2
             | Hyphen xi, _ ->
                 v_badness
                   ((xi.hyphen_normal, 0, Array.length xi.hyphen_normal)::(hi, i+1, maxi)::si) 0
                   boxes_j j
                   w_tot col col2
             | _, Hyphen xj ->
                 v_badness
                   boxes_i i
                   ((xj.hyphen_normal, 0, Array.length xj.hyphen_normal)::(hj, j+1, maxj)::sj) 0
                   w_tot col col2
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
                 let wi=box_width comp0 box_i in
                 let wj=box_width comp1 box_j in
                   if (!xi +.wi < !xj+. wj && boxes_i<>[]) || boxes_j=[] then (
                     let yi_=lower_y box_i wi in
                     let yi=if yi_=infinity then 0. else yi_ in
                     let yj_=if !xi+.wi < !xj then 0. else upper_y box_j wj in
                     let yj=if yj_=(-.infinity) then 0. else yj_ in
                     let x0=if !xi+.wi < !xj then !xi else max !xi !xj in
                     let w0= !xi +. wi -. x0 in
                       xi:= !xi+.wi;
                       if !xj>= !xi+.wi || is_glue box_i || is_glue box_j then
                         (v_badness boxes_i (i+1) boxes_j j w_tot col col2)
                       else
                         (let area=w0*.(v_space+.yi-.yj) in
                            v_badness boxes_i (i+1) boxes_j j (w_tot+.w0) (col+.area) (col+.area*.area))
                   ) else (
                     let yi_=if !xj > !xi +. wi then 0. else lower_y box_i wi in
                     let yi=if yi_=(infinity) then 0. else yi_ in
                     let yj_=upper_y box_j wj in
                     let yj=if yj_=(-.infinity) then 0. else yj_ in

                     let x0=if !xj+.wj < !xi then !xj else max !xi !xj in
                     let w0= !xj +. wj -. x0 in
                       xj:= !xj +. w0;
                       if !xi>= !xj+.wj || is_glue box_i || is_glue box_j then
                         (v_badness boxes_i i boxes_j (j+1) w_tot col col2)
                       else
                         (let area=w0*.(v_space+.yi-.yj) in
                            v_badness boxes_i i boxes_j (j+1) (w_tot+.w0) (col+.area) (col+.area*.area))
                   )
               )
          )
  in
  let li0=
    (paragraphs.(node0.paragraph), (if node0.hyphenStart>=0 then node0.lineStart+1 else node0.lineStart), node0.lineEnd)::
      (if node0.hyphenEnd>=0 then
         (match paragraphs.(node0.paragraph).(node0.lineEnd) with
              Hyphen x->let hyp=fst x.hyphenated.(node0.hyphenEnd) in [(hyp, 0, Array.length hyp)]
            | _->[])
       else [])
  in
  let li=
    (if node0.hyphenStart>=0 then
       (match paragraphs.(node0.paragraph).(node0.lineStart) with
            Hyphen x->let hyp=snd x.hyphenated.(node0.hyphenStart) in (hyp, 0, Array.length hyp)::li0
          | _->li0)
     else li0)
  in
  let lj0=
    (paragraphs.(node1.paragraph), (if node1.hyphenStart>=0 then node1.lineStart+1 else node1.lineStart), node1.lineEnd)::
      (if node1.hyphenEnd>=0 then
         (match paragraphs.(node1.paragraph).(node1.lineEnd) with
              Hyphen x->let hyp=fst x.hyphenated.(node1.hyphenEnd) in [(hyp,0,Array.length hyp)]
            | _->[])
       else [])
  in
  let lj=
    (if node1.hyphenStart>=0 then
       (match paragraphs.(node1.paragraph).(node1.lineStart) with
            Hyphen x->(let hyp=snd x.hyphenated.(node1.hyphenStart) in (hyp,0,Array.length hyp)::lj0)
          | _->lj0)
     else lj0)
  in
    match li, lj with
        (_,i,_)::_,(_,j,_)::_->
          v_badness li i lj j 0. 0. 0.
      | _->failwith "impossible case"

let h_badness paragraphs node comp=
  let bad=ref 0. in
    for k=node.lineStart to node.lineEnd-1 do
      bad:= !bad +.
        (match paragraphs.(node.paragraph).(k) with
             Drawing x
           | Glue x->x.drawing_badness (x.drawing_min_width+.(x.drawing_max_width-.x.drawing_min_width)*.comp)
           | _->0.
        )
    done;
    !bad



let badness paragraphs ?figures:(figures=[||]) ?citations:(citations=[||]) node params nextNode params'=
  if nextNode.paragraph>=Array.length paragraphs then 0. else (
    let comp0=compression paragraphs (params,node) in
    let comp1=compression paragraphs (params',nextNode) in
    let v_bad=
      if node.page=nextNode.page then (
        v_badness paragraphs
          (float_of_int (nextNode.height-node.height)*.params.lead)
          node params.left_margin comp0
          nextNode params'.left_margin comp1
      ) else 0.
    in
    let figure_badness=
      (* Pour toutes les figures déjà placées *)
      let bad=ref 0. in
      let fig_after=
        if node.page<>nextNode.page then 5000. else
          if node.paragraph<>nextNode.paragraph then 10000. else 0.
      in
        for i=nextNode.lastFigure+1 to Array.length figures-1 do
          if i<Array.length citations then (
            if citations.(i) < (nextNode.paragraph, nextNode.lineStart) then
              bad:=(!bad) +. fig_after
          )
        done;

        let fig_before=
          if node.page<>nextNode.page then 5000. else
            if node.paragraph<>nextNode.paragraph then 10000. else 0.
        in
          for i=0 to nextNode.lastFigure do
            if i<Array.length citations then (
              if citations.(i) >= (nextNode.paragraph, nextNode.lineStart) &&
                (not nextNode.isFigure || i=nextNode.lastFigure)
              then
                bad:=(!bad) +. fig_before
            )
          done;
        !bad
    in
      (h_badness paragraphs nextNode comp1)
      +. v_bad
      +. figure_badness
        (* Page pas assez remplie *)
      +. (if node.page<>nextNode.page && node.height<>params.lines_by_page-1 then 1e20 else 0.)
        (* Cesures *)
      +. (if nextNode.hyphenEnd >=0 then
            (if nextNode.hyphenStart >=0 then
               1e31
             else
               1e30)
          else
            0.)
      +. (1000.*.(abs_float (comp0-.comp1)))
  )
