open Util


let v_badness v_space haut max_haut params_i comp_i bas max_bas params_j comp_j=

  let xi=ref params_i.left_margin in
  let xj=ref params_j.left_margin in

  let rec collide i j w_tot col col2=
    let box_i=haut.(i) in
    let box_j=bas.(j) in
      (* let _=Graphics.wait_next_event [Graphics.Key_pressed] in *)
    let wi=box_width comp_i box_i in
    let wj=box_width comp_j box_j in
      if !xi +.wi < !xj+. wj && i < max_haut then (
        let yi=lower_y box_i wi in
        let yj=if !xi+.wi < !xj then -.infinity else
          if upper_y box_j wj > -.infinity then upper_y box_j wj else 0.
        in
        let x0=if !xi+.wi < !xj then !xi else max !xi !xj in
        let w0= !xi +. wi -. x0 in

          xi:= !xi+.wi;
          if !xj>= !xi+.wi || is_glue box_i || is_glue box_j then
            (collide (i+1) j w_tot col col2)
          else
            (let area=w0*.(v_space+.yi-.yj) in
               collide (i+1) j (w_tot+.w0) (col+.area) (col+.area*.area))
      ) else if j < max_bas then (
        let yi=if !xj +. wj < !xi then infinity else
          if lower_y box_i wi < infinity then lower_y box_i wi else 0. in
        let yj=upper_y box_j wj in
        let x0=if !xj+.wj < !xi then !xj else max !xi !xj in
        let w0= !xj +. wj -. x0 in
          xj:= !xj+.wj;
          if !xi>= !xj+.wj || is_glue box_i || is_glue box_j then
            (collide i (j+1) w_tot col col2)
          else
            (let area=w0*.(v_space+.yi-.yj) in
               collide i (j+1) (w_tot+.w0) (col+.area) (col+.area*.area))
      ) else (if w_tot<=0. then 0. else ((col*.col-.col2)/.w_tot))
  in
    collide 0 0 0. 0. 0.




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



let badness paragraphs ?figures:(figures=[||])
    node_i line_i max_i params_i comp_i
    node_j line_j max_j params_j comp_j=

  if node_j.paragraph>=Array.length paragraphs then 0. else (
    let v_bad=
      if node_i.page=node_j.page then (
        v_badness
          (node_j.height-.node_i.height)
          line_i max_i params_i comp_i
          line_j max_j params_j comp_j
      ) else 0.
    in
      (h_badness paragraphs node_j comp_j)
      +. v_bad
        (* Page pas assez remplie *)
      +. (if node_i.page<>node_j.page &&
            node_i.height<>params_i.page_height then 10000. else 0.)
        (* Cesures *)
      +. (if node_j.hyphenEnd >=0 then
            (if node_j.hyphenStart >=0 then
               1e31
             else
               10000.)
          else
            0.)
      +. (1000.*.(abs_float (comp_i-.comp_j)))
  )
