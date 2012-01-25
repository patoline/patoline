#define DEBUG_BOXES 1

open Drivers
open Binary
open Constants
open CamomileLibrary
open Util


exception Impossible

let isGlue x=match x with Glue _->true | _->false

type line= { paragraph:int; lineStart:int; lineEnd:int; lastFigure:int; height:int;
             paragraph_height:int
           }

let print_line l=
#ifdef DEBUG_BOXES
  Printf.printf "{ paragraph=%d; lineStart=%d; lineEnd=%d; lastFigure=%d; height=%d }\n"
  l.paragraph l.lineStart l.lineEnd l.lastFigure l.height
#else
  ()
#endif

let print_text_line lines node=

#ifdef DEBUG_BOXES
  print_line node;
  for i=node.lineStart to node.lineEnd-1 do
    match lines.(node.paragraph).(i) with
        Glue _->Printf.printf " "
      | GlyphBox x->Printf.printf "%s" (x.contents)
      | _->Printf.printf "[]"
  done;
  print_newline();
#else
  ()
#endif



module LineMap=Map.Make (struct type t=line let compare=compare end)


type parameters={ format:float*float;
                  lead:float;
                  measure:float;
                  line_height:int }

let box_width comp=function
    GlyphBox x->x.width*.x.size/.1000.
  | Glue x->(x.glue_min_width+.(x.glue_max_width-.x.glue_min_width)*.comp)
  | Drawing x->(x.drawing_min_width+.(x.drawing_max_width-.x.drawing_min_width)*.comp)
  | _->0.

let lower_y x w=match x with
    GlyphBox y->y.y0*.y.size/.1000.
  | Drawing y->y.drawing_y0 w
  | Glue _->0.
  | _->0.

let upper_y x w=match x with
    GlyphBox y->y.y1*.y.size/.1000.
  | Drawing y->y.drawing_y1 w
  | Glue _->0.
  | _-> 0.



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
          (match get i j with
               Glue x->(bmin:= !bmin+.x.glue_min_width;bmax:= !bmax+.x.glue_max_width)
             | GlyphBox x->(let w=x.width*.x.size/.1000. in bmin:= !bmin+.w;bmax:= !bmax+.w)
             | Drawing x->(bmin:= !bmin+.x.drawing_min_width;bmax:= !bmax+.x.drawing_max_width)
             | _->()
          );
        done;
        length_min.(i).(Array.length (Array.get lines i))<- !bmin;
        length_max.(i).(Array.length (Array.get lines i))<- !bmax;
      done
  in

  let compression m p i j=
    let minLine=length_min.(p).(j) -. length_min.(p).(i) in
    let maxLine=length_max.(p).(j) -. length_max.(p).(i) in
      min 1. ((m-.minLine)/.(maxLine-. minLine))
        
  in

  let rec collide pi comp_i max_i pj comp_j max_j i xi j xj max_col=
    if (i>=max_i && j>=max_j) then
      max_col
    else (
      (* let _=Graphics.wait_next_event [Graphics.Key_pressed] in *)
      let wi=if i<max_i then box_width comp_i lines.(pi).(i) else 0. in
      let wj=if j<max_j then box_width comp_j lines.(pj).(j) else 0. in
        if (xi +.wi < xj+. wj || j>=max_j) && i<max_i then (
          let yi=if i>=max_i then 0. else lower_y lines.(pi).(i) wi in
          let yj=if xi+.wi<xj || j>=max_j then 0. else upper_y lines.(pj).(j) wj in
          let x0=if xi+.wi<xj then xi else max xi xj in
          let w0=xi +. wi -. x0 in
            if w0>=0. then (
              (* Graphics.set_color Graphics.red; *)
              (* Graphics.draw_rect (round (mm*.x0)) (round (20.+.mm*.yj)) (round (mm*.w0)) (round (80.+.mm*.(yi-.yj))); *)
              (* Graphics.set_color Graphics.black; *)
              (* let _=Graphics.wait_next_event [Graphics.Key_pressed] in *)
            );
            collide pi comp_i max_i pj comp_j max_j (i+1) (xi+.wi) j xj (min max_col (yi-.yj))
              
        ) else (
          let yi=if xj>xi+.wi || i>=max_i then 0. else lower_y lines.(pi).(i) wi in
          let yj=if j>=max_j then 0. else upper_y lines.(pj).(j) wj in
          let x0=if xj+.wj<xi then xj else max xi xj in
          let w0=xj +. wj -. x0 in
            if w0>=0. then (
              (* Graphics.set_color Graphics.green; *)
              (* Printf.printf "%d %d %d %d\n"(round (mm*.x0)) (round (200.+.mm*.yj)) (round (mm*.w0)) (round (80.+.mm*.(yi-.yj))); *)
              (* Graphics.draw_rect (round (mm*.x0)) (round (20.+.mm*.yj)) (round (mm*.w0)) (round (80.+.mm*.(yi-.yj))); *)
              (* Graphics.set_color Graphics.black; *)
              (* let _=Graphics.wait_next_event [Graphics.Key_pressed] in *)
            );
            collide pi comp_i max_i pj comp_j max_j i xi (j+1) (xj+.wj) (min max_col (yi-.yj))
        )
    )
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
    let v_badness v_space node0 node1=
      (* Calcul de la moyenne de collision *)
      let pi=node0.paragraph in
      let pj=node1.paragraph in
      let comp0=compression parameters.measure pi node0.lineStart node0.lineEnd in
      let comp1=compression parameters.measure pj node1.lineStart node1.lineEnd in
      let rec mean_collide i xi j xj w_tot col col2=
        if i>=node0.lineEnd || j>=node1.lineEnd then
          (if w_tot<=0. then 0. else (col2 -. col*.col)/.w_tot)
        else (
          let wi=box_width comp0 lines.(pi).(i) in
          let wj=box_width comp1 lines.(pj).(j) in
            if xi +.wi < xj+. wj then (
              let x0=max xi xj in
              let w0=xi+.wi-.x0 in
              let yi=lower_y lines.(pi).(i) wi in
              let yj=upper_y lines.(pj).(j) wj in

                if xj>=xi+.wi || isGlue lines.(pi).(i) || isGlue lines.(pj).(j) then
                  mean_collide (i+1) (xi+.w0) j xj w_tot col col2
                else
                  let area=w0*.(v_space+.yi-.yj) in
                    mean_collide (i+1) (xi+.w0) j xj (w_tot+.w0) (col+.area) (col+.area*.area)

            ) else (
              let x0=max xi xj in
              let w0=xj+.wj-.x0 in
              let yi=lower_y lines.(pi).(i) wi in
              let yj=upper_y lines.(pj).(j) wj in

                if xi>=xj+.wj || isGlue lines.(pi).(i) || isGlue lines.(pj).(j) then
                  mean_collide i xi j (xj+.w0) w_tot col col2
                else
                  let area=w0*.(v_space+.yi-.yj) in
                    mean_collide i xi j (xj+.w0) (w_tot+.w0) (col+.area) (col+.area*.area)
            )
        )
      in
        
        mean_collide node0.lineStart 0. node1.lineStart 0. 0. 0. 0.

    in
        
      (* A chaque etape, todo contient le dernier morceau de chemin qu'on a construit dans demerits *)
      if LineMap.is_empty todo then demerits else
      (
        let node,lastBadness=LineMap.min_binding todo in
        let todo'=ref (LineMap.remove node todo) in
          if node.paragraph >= Array.length lines then break parameters !todo' demerits else
            (
              (*Printf.printf "node : ";print_line node;*)
              (* On commence par chercher la première vraie boite après node *)
              let i=ref node.lineEnd in
              let demerits'=ref demerits in
              let register node nextNode badness=
                Printf.printf "register\n";
                print_line nextNode;
                let reallyAdd ()=
                  Printf.printf "Added %f !\n" badness;
                  todo':=LineMap.add nextNode badness !todo';
                  demerits':=LineMap.add nextNode (badness,node) !demerits'
                in
                  try
                    let bad,_=LineMap.find nextNode !demerits' in
                      if bad>badness then reallyAdd () else
                        (Printf.printf "bad=%f, badness=%f\n" bad badness; flush stdout)
                  with
                      Not_found->reallyAdd ()
              in

                while !i< Array.length (Array.get lines node.paragraph) &&
                  (match get node.paragraph !i with GlyphBox _->false | _->true) do
                  incr i
                done;

                (* Y a-t-il encore des boites dans ce paragraphe ? *)
                if !i>=Array.length (Array.get lines node.paragraph) then (

                  (* On s'apprete a changer de paragraphe.
                     - On ne peut pas le faire si on est une ou deux lignes avant la fin de la page
                     - Ni si on est juste une ligne apres le debut.
                     - Si on est a la fin de la page, il faut enlever une ligne.
                  *)
                  
                  if (node.height < parameters.line_height-3 || node.height=parameters.line_height-1) &&
                    (node.height >=1 || node.paragraph_height<=1) then

                      (let nextNode={ 
                         paragraph=node.paragraph+1; lastFigure=node.lastFigure;
                         height=
                           if (node.height=parameters.line_height-1 ||
                               node.paragraph+1>=Array.length lines) then
                             -1
                           else
                             node.height+1;
                         lineStart= 0; lineEnd= 0; paragraph_height= -1 }
                       in
                       let badness'=lastBadness in
                         register node nextNode badness');

                  break parameters !todo' !demerits'

                ) else (
                  
                  (* Ensuite, on cherche toutes les coupes possibles. Cas particulier : la fin du paragraphe. *)
                  let j=ref (!i+1) in
                    while !j <= (Array.length (Array.get lines node.paragraph)) && 
                      (length_min.(node.paragraph).(!j) -. length_min.(node.paragraph).(!i)) <= parameters.measure do
                        
                      (if !j=Array.length (Array.get lines node.paragraph) ||
                         (length_max.(node.paragraph).(!j) -. length_max.(node.paragraph).(!i) >= parameters.measure && 
                            isGlue (get node.paragraph !j)) then
                           (let comp0=compression parameters.measure node.paragraph node.lineStart node.lineEnd in
                            let comp1=compression parameters.measure node.paragraph !i !j in
                            let v_distance= if node.height+1>=parameters.line_height then 0. else
                              collide node.paragraph comp0 node.lineEnd node.paragraph comp1 !j
                                node.lineStart 0. !i 0. infinity
                            in
                              
                            let v_incr=int_of_float (ceil (max 1. (-.v_distance/.parameters.lead))) in
                              Printf.printf "v_incr=%f\n" (-.v_distance/.parameters.lead);
                              if node.height+v_incr<parameters.line_height || v_incr=1 then (
                                let nextNode={ paragraph=node.paragraph; lastFigure=node.lastFigure;
                                               height=(node.height+v_incr) mod parameters.line_height;
                                               lineStart= !i; lineEnd= !j;
                                               paragraph_height=node.paragraph_height+1 }
                                in
                                let v_bad=v_badness (float_of_int v_incr*.parameters.lead) node nextNode in
                                let bad=(lastBadness+. v_bad*.v_bad +.
                                           (h_badness node.paragraph !i !j)) in
                                  Printf.printf "\nVertical badness\n";
                                  print_text_line lines node;
                                  print_text_line lines nextNode;
                                  Printf.printf "v_badness = %f, h_badness = %f\n" v_bad (h_badness node.paragraph !i !j);
                                  Printf.printf "badness = %f\n" bad;
                                  register node nextNode bad
                              )
                           )
                      );
                      incr j
                    done;
                    break parameters !todo' !demerits'
                )
            )
      )
  in
  let todo=LineMap.singleton { paragraph=0; lineStart=0; lineEnd=0; lastFigure=(-1); height= -1;paragraph_height= -1 } 0. in
  let demerits=break parameters0 todo (LineMap.empty) in
  let (b,(bad,_)) = LineMap.max_binding demerits in

    if b.paragraph<Array.length lines then
      raise Impossible
    else (
      let rec makeLines node result=
        (* print_text_line lines node; *)
        try  
          let _,next=LineMap.find node demerits in
            makeLines next (node::result)
        with
            Not_found->if node.paragraph>0 || node.height>=0 then raise Impossible else result
      in
      let makeLine node y=
        let minLine=length_min.(node.paragraph).(node.lineEnd) -. length_min.(node.paragraph).(node.lineStart) in
        let maxLine=length_max.(node.paragraph).(node.lineEnd) -. length_max.(node.paragraph).(node.lineStart) in
        let compression=min 1. ((parameters0.measure-.minLine)/.(maxLine-. minLine)) in
        let rec makeLine x i line=
          if i>=node.lineEnd then line else
            match get node.paragraph i with
                Glue g->let w=g.glue_min_width+.compression*.(g.glue_max_width-.g.glue_min_width) in
                  makeLine (x+.w) (i+1) line
              | box->makeLine (x+.(box_width compression box)) (i+1) ((x,y,box)::line)
        in
          makeLine 0. node.lineStart []
      in
        
      let rec makePages p pages=match p with
          []->pages
        | node::s ->(
            print_text_line lines node;
            let pages'=if node.height=0 || (match pages with []->true | _->false) then
              (Array.create parameters0.line_height [])::pages else pages in
            let first=List.hd pages' in
              Printf.printf "node.height=%d\n" node.height; flush stdout;
              if node.lineEnd > node.lineStart && node.height>=0 then
                first.(node.height)<-makeLine node (float_of_int node.height *. parameters0.lead);
              
              makePages s pages'
          )
      in
      let ln=(makeLines b []) in
        Array.of_list (List.rev (makePages ln []))
    )
