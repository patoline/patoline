open Drivers
open Binary
open Constants
open CamomileLibrary
open Util
open FontsTypes

exception Impossible


type line= { paragraph:int; lineStart:int; lineEnd:int; hyphenStart:int; hyphenEnd:int;
             lastFigure:int; height:int; paragraph_height:int }

let print_line l=
  Printf.printf "{ paragraph=%d; lineStart=%d; lineEnd=%d; hyphenStart=%d; hyphenEnd=%d; lastFigure=%d; height=%d }\n"
  l.paragraph l.lineStart l.lineEnd l.hyphenStart l.hyphenEnd l.lastFigure l.height

let print_text_line lines node=
  print_line node;
  for i=node.lineStart to node.lineEnd-1 do
    let rec print_box=function
        Glue _->Printf.printf " "
      | GlyphBox (_,x)->Printf.printf "%s" (x.contents)
      | Kerning x->print_box x.kern_contents
      | Hyphen x->Array.iter print_box x.hyphen_normal
      | _->Printf.printf "[]"
    in
      print_box (lines.(node.paragraph).(i))
  done;
  print_newline()


module LineMap=Map.Make (struct type t=line let compare=compare end)

let rec print_graph file lines graph=
  let f=open_out file in
    Printf.fprintf f "digraph {\n";
    LineMap.iter (fun k (_,a)->
                    Printf.fprintf f "node_%d_%s_%s [label=\"%d, %d\"];\n"
                      k.paragraph (if k.lineStart>=0 then string_of_int k.lineStart else "x")
                      (if k.lineEnd>=0 then string_of_int k.lineEnd else "x")
                      k.lineStart k.lineEnd;
                    
                    Printf.fprintf f "node_%d_%s_%s -> node_%d_%s_%s\n"
                      a.paragraph (if a.lineStart>=0 then string_of_int a.lineStart else "x")
                      (if a.lineEnd>=0 then string_of_int a.lineEnd else "x")
                      k.paragraph (if k.lineStart>=0 then string_of_int k.lineStart else "x")
                      (if k.lineEnd>=0 then string_of_int k.lineEnd else "x")
                 ) graph;
    Printf.fprintf f "};\n";
    close_out f

type parameters={ format:float*float;
                  lead:float;
                  measure:float;
                  line_height:int }




let lineBreak parameters0 ?figures:(figures = [||]) lines=
  
  let get i j=lines.(i).(j) in
    
  let length_min=Array.create (Array.length lines) [||] in
  let length_max=Array.create (Array.length lines) [||] in
  let _=
    let bmin=ref 0. in
    let bmax=ref 0. in
      for i=0 to Array.length lines-1 do
        length_min.(i)<-Array.create (Array.length (Array.get lines i)+1) 0.;
        length_max.(i)<-Array.create (Array.length (Array.get lines i)+1) 0.;
        for j=0 to Array.length (Array.get lines i)-1 do
          length_min.(i).(j)<- !bmin;
          length_max.(i).(j)<- !bmax;
          let a,b=box_interval (get i j) in
            bmin:= !bmin+.a;
            bmax:= !bmax+.b
        done;
        length_min.(i).(Array.length (Array.get lines i))<- !bmin;
        length_max.(i).(Array.length (Array.get lines i))<- !bmax;
      done
  in

  let compression m p i hi j hj=
    let minLine=ref 0. in
    let maxLine=ref 0. in
      if hi>=0 then (
        match lines.(p).(i-1) with
            Hyphen x->let a,b=boxes_interval (snd x.hyphenated.(hi)) in
              (minLine:= !minLine+.a;
               maxLine:= !maxLine+.b)
          | _->());
      if hj>=0 then (
        match lines.(p).(j) with
            Hyphen x->let a,b=boxes_interval (fst x.hyphenated.(hj)) in
              (minLine:= !minLine+.a;
               maxLine:= !maxLine+.b)
          | _->());
      for k=i to j-1 do
        let a,b=box_interval lines.(p).(k) in
          minLine := !minLine+.a;
          maxLine := !maxLine+.b
      done;
      min 1. ((m-. !minLine)/.(!maxLine-. !minLine))
  in
  let make_line pi lineStart lineEnd hyphenStart hyphenEnd=
    let arr0=
      (try
         match lines.(pi).(lineStart-1) with
             Hyphen x->fst (x.hyphenated.(hyphenStart))
           | _->[||]
       with
           _->[||])
    in
    let arr1=
      (try
         match lines.(pi).(lineEnd) with
             Hyphen x->fst (x.hyphenated.(hyphenEnd))
           | _->[||]
       with
           _->[||])
    in
      Array.append (Array.append arr0 (
                      if lineStart<lineEnd then
                        Array.sub lines.(pi) lineStart (lineEnd-lineStart)
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
                        (* Printf.printf "%d %d %d %d\n"(round (mm*.x0)) (round (200.+.mm*.yj)) (round (mm*.w0)) (round (80.+.mm*.(yi-.yj))); *)
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
    
  let rec break parameters todo demerits=
   
    let h_badness pi i j=
      let min_l=length_min.(pi).(j)-.length_min.(pi).(i) in
      let max_l=length_max.(pi).(j)-.length_max.(pi).(i) in
      let comp=if max_l=min_l then 0. else (parameters.measure-.min_l) /. (max_l-.min_l) in

      let bad=ref 0. in
        for k=i to j-1 do
          bad:= !bad +.
            (match lines.(pi).(i) with
                 Glue x->x.glue_badness (x.glue_min_width+.(x.glue_max_width-.x.glue_min_width)*.comp)
               | Drawing x->x.drawing_badness (x.drawing_min_width+.(x.drawing_max_width-.x.drawing_min_width)*.comp)
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
      if LineMap.is_empty todo then demerits else
      (
        let node,lastBadness=LineMap.min_binding todo in
        let todo'=ref (LineMap.remove node todo) in
          if node.paragraph >= Array.length lines then break parameters !todo' demerits else
            (
              (* On commence par chercher la première vraie boite après node *)
              let demerits'=ref demerits in
              let register node nextNode badness=
                let reallyAdd ()=
                  todo':=LineMap.add nextNode badness !todo';
                  demerits':=LineMap.add nextNode (badness,node) !demerits'
                in
                  try
                    let bad,_=LineMap.find nextNode !demerits' in
                      if bad>badness then reallyAdd ()
                  with
                      Not_found->reallyAdd ()
              in
              let i=node.lineEnd+1 in
                (* Y a-t-il encore des boites dans ce paragraphe ? *)
                if i>=Array.length (Array.get lines node.paragraph) then (
                  
                  (* On s'apprete a changer de paragraphe.
                     - On ne peut pas le faire si on est une ou deux lignes avant la fin de la page
                     - Ni si on est juste une ligne apres le debut.
                     - Si on est a la fin de la page, il faut enlever une ligne.
                  *)
                  
                  (* if (node.height < parameters.line_height-3 || node.height=parameters.line_height-1) && *)
                  (*   (node.height >=1 || node.paragraph_height<=1) then *)

                  (let nextNode={ 
                     paragraph=node.paragraph+1; lastFigure=node.lastFigure;
                     hyphenStart= -1; hyphenEnd= -1;
                     height=
                       if (node.height=parameters.line_height-1 ||
                           node.paragraph+1>=Array.length lines) then
                         -1
                       else
                         node.height+1;
                     lineStart= -1; lineEnd= -1; paragraph_height= -1 }
                   in
                   let badness'=lastBadness in
                     register node nextNode badness');
                  
                  break parameters !todo' !demerits'

                ) else (
                  
                  (* Ensuite, on cherche toutes les coupes possibles. Cas particulier : la fin du paragraphe. *)
                  let rec break_next j sum_min sum_max=
                    let make_next_node hyphen=
                      let v_badness,v_incr=
                        if node.height>=0 then (
                          let line0=make_line node.paragraph node.lineStart node.lineEnd node.hyphenStart node.hyphenEnd in
                          let line1=make_line node.paragraph i j node.hyphenEnd hyphen in
                          let comp0=compression parameters.measure node.paragraph node.lineStart node.hyphenStart
                            node.lineEnd node.hyphenEnd in
                          let comp1=compression parameters.measure node.paragraph i node.hyphenEnd j hyphen in
                            
                          let v_distance= if node.height+1>=parameters.line_height then 0. else
                            collide line0 comp0 0. line1 comp1 0.
                          in
                          let v_incr=int_of_float (ceil (max 1. (-.v_distance/.parameters.lead))) in
                            v_badness (float_of_int v_incr*.parameters.lead) line0 comp0 line1 comp1, v_incr
                        ) else
                          (0.,1)
                      in
                        if node.height+v_incr<parameters.line_height || v_incr=1 then (
                          let nextNode={ paragraph=node.paragraph; lastFigure=node.lastFigure;
                                         hyphenStart= node.hyphenEnd; hyphenEnd= hyphen;
                                         height=(node.height+v_incr) mod parameters.line_height;
                                         lineStart= i; lineEnd= j;
                                         paragraph_height=node.paragraph_height+1 }
                          in
                          let bad=(lastBadness+. v_badness*.v_badness +.
                                     (h_badness node.paragraph i j)) in
                            register node nextNode bad
                        )
                    in
                      
                      if j>=Array.length (lines.(node.paragraph)) then make_next_node (-1) else
                        match lines.(node.paragraph).(j) with
                            Hyphen x->(
                              for i=0 to Array.length x.hyphenated-1 do
                                let a,b=boxes_interval (fst x.hyphenated.(i)) in
                                  if sum_min+.a <= parameters.measure && sum_max+.b >= parameters.measure then
                                    make_next_node i
                              done;
                              let a,b=boxes_interval x.hyphen_normal in
                                break_next (j+1) (sum_min+. a) (sum_max+. b)
                            )
                          | _ when sum_min <= parameters.measure->(
                              (if sum_max >= parameters.measure then
                                 match lines.(node.paragraph).(j) with
                                     Glue _->make_next_node (-1)
                                   | _->());
                                       
                              let a,b=box_interval lines.(node.paragraph).(j) in
                                break_next (j+1) (sum_min+. a) (sum_max+. b)
                            )
                          | _->()
                  in
                    break_next (i+1) 0. 0.;
                    break parameters !todo' !demerits'
                )
            )
      )
  in
  let todo=LineMap.singleton { paragraph=0; lineStart= -1; lineEnd= -1; hyphenStart= -1; hyphenEnd= -1;
                               lastFigure=(-1); height= -1;paragraph_height= -1 } 0. in
  let demerits=break parameters0 todo (LineMap.empty) in

    print_graph "graph" lines demerits;
    
  let (b,(bad,_)) = LineMap.max_binding demerits in

    if b.paragraph<Array.length lines then
      (Printf.printf "impossible"; flush stdout; raise Impossible)
    else (
      let rec makeLines node result=
        try  
          let _,next=LineMap.find node demerits in
            makeLines next (node::result)
        with
            Not_found->if node.paragraph>0 || node.height>=0 then raise Impossible else result
      in
      let makeLine node y=
        let comp=compression parameters0.measure node.paragraph node.lineStart node.hyphenStart node.lineEnd node.hyphenEnd in
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
        let u,v=(if node.hyphenStart>=0 then match lines.(node.paragraph).(node.lineStart-1) with
                     Hyphen x->let _,y=x.hyphenated.(node.hyphenStart) in
                       makeLine y 0. 0 (Array.length y) []
                   | _->0., []
                 else 0.,[])
        in
        let u',v'=makeLine lines.(node.paragraph) u node.lineStart node.lineEnd v in
          if node.hyphenEnd>=0 then match lines.(node.paragraph).(node.lineEnd) with
              Hyphen x->let y,_=x.hyphenated.(node.hyphenEnd) in snd (makeLine y u' 0 (Array.length y) v')
            | _->v'
          else
            v'
      in
        
      let rec makePages p pages=match p with
          []->pages
        | node::s ->(
            let pages'=if node.height=0 || (match pages with []->true | _->false) then
              (Array.create parameters0.line_height [])::pages else pages in
            let first=List.hd pages' in
              (* Printf.printf "node.height=%d\n" node.height; flush stdout; *)
              if node.lineEnd > node.lineStart && node.height>=0 then
                first.(node.height)<-
                  makeLine node (float_of_int node.height *. parameters0.lead);
              
              makePages s pages'
          )
      in
      let ln=(makeLines b []) in
        Array.of_list (List.rev (makePages ln []))
    )
