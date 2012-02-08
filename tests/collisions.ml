open Drivers
open Binary
open Boxes
open Constants
open Lexing
open Util
open CamomileLibrary

module M=Output.Routine(Pdf)

let spec = []

exception Syntax_Error of Lexing.position * string

let default={ format=a4; lead=3.; measure=150.; lines_by_page=30; left_margin=10. }


let test=
  "Ceci est un petit texte sympa, avec un premier paragraphe assez long "^
  "pour avoir des sauts de lignes : avec même des accents, surtout l'été !"

let mm=8.


let draw_glyph (x0,y0) (size,gl)=
  let fact=mm*.size/.1000. in
  List.iter (fun (x,y)->
               if Array.length x = 2 then
                 (let (a,b)=x.(0),y.(0) in
                  let (c,d)=x.(1),y.(1) in
                    Graphics.moveto (x0+round (a*.fact)) (y0+round (b*.fact));
                    Graphics.lineto (x0+round (c*.fact)) (y0+round (d*.fact)))
               else
                 (let (a,b)=x.(0),y.(0) in
                  let (c,d)=x.(1),y.(1) in
                  let (e,f)=x.(2),y.(2) in
                  let (g,h)=x.(3),y.(3) in
                    Graphics.moveto (x0+round (a*.fact)) (y0+round (b*.fact));
                    Graphics.curveto (x0+round (c*.fact), y0+round (d*.fact))
                      (x0+round (e*.fact), y0+round (f*.fact)) (x0+round (g*.fact), y0+round (h*.fact)))
            ) (Fonts.outlines gl.glyph)



let draw_line (x0,y0) pages node comp=
  let x=ref (float_of_int x0) in
  let y=ref y0 in

  let rec draw boxes j maxj=
    if boxes==pages.(node.paragraph) && j<=node.lineStart && node.hyphenStart>=0 then (
      (match boxes.(j) with
           Hyphen h->let hyp=snd h.hyphenated.(node.hyphenStart) in draw hyp 0 (Array.length hyp)
         | _->());
      draw boxes (j+1) maxj
    ) else (
      if j>maxj then () else
        if j=maxj then (
          if boxes==pages.(node.paragraph) && j=maxj && node.hyphenEnd>=0 then (
            (match boxes.(j) with
                 Hyphen h->let hyp=fst h.hyphenated.(node.hyphenEnd) in draw hyp 0 (Array.length hyp)
               | _->());
          )
        ) else (
          match boxes.(j) with
              GlyphBox (size,gl)->(
                Graphics.draw_rect
                  (round !x) (!y+round (mm*.gl.y0*.size/.1000.))
                  (round (mm*.gl.width*.size/.1000.)) (round (mm*.(gl.y1-.gl.y0)*.size/.1000.));
                draw_glyph (round !x, !y) (size,gl);
                x:= !x +. (mm*.gl.width*.size/.1000.);
                draw boxes (j+1) maxj
              )
            | Glue gl->(
                x:= !x +. (mm*.(gl.glue_min_width+.(gl.glue_max_width-.gl.glue_min_width)*.comp));
                draw boxes (j+1) maxj
              )
            | Hyphen x->(
                draw x.hyphen_normal 0 (Array.length x.hyphen_normal);
                draw boxes (j+1) maxj
              )
            | _->draw boxes (j+1) maxj
        )
    )
  in
    draw pages.(node.paragraph) node.lineStart node.lineEnd

let inf_zero x=if x=infinity || x= -.infinity then 0. else x

let is_hyphen=function Hyphen _->true | _->false

let collide_ paragraphs
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
            print_box box_j;print_box box_i; print_newline();flush stdout;
          let wi=box_width comp_i box_i in
          let wj=box_width comp_j box_j in
            if (!xi +.wi < !xj+. wj && boxes_i<>[]) || boxes_j=[] then (
              let yi=lower_y box_i wi in
              let yj=if !xi+.wi < !xj then 0. else upper_y box_j wj in
              let x0=if !xi+.wi < !xj then !xi else max !xi !xj in
              let w0= !xi +. wi -. x0 in
                if w0>=0. then (
                  Graphics.set_color Graphics.red;
                  Graphics.draw_rect (round (mm*.x0)) (round (20.+.mm*.yj)) (round (mm*.w0)) (round (80.+.mm*.(yi-.yj)));
                  Graphics.set_color Graphics.black;
                  (* let _=Graphics.wait_next_event [Graphics.Key_pressed] in () *)
                );
                xi:= !xi+.wi;
                collide boxes_i (i+1) boxes_j j (min max_col (yi-.yj))
            ) else (
              let yi=if !xj > !xi +. wi then 0. else lower_y box_i wi in
              let yj=upper_y box_j wj in
              let x0=if !xj+.wj < !xi then !xj else max !xi !xj in
              let w0= !xj +. wj -. x0 in
                if w0>=0. then (
                  Graphics.set_color Graphics.green;
                  Graphics.draw_rect (round (mm*.x0)) (round (20.+.mm*.yj)) (round (mm*.w0)) (round (80.+.mm*.(yi-.yj)));
                  Graphics.set_color Graphics.black;
                  (* let _=Graphics.wait_next_event [Graphics.Key_pressed] in () *)
                );
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
            Hyphen x->(Printf.printf "hyphen'\n";flush stdout;
                       let hyp=snd x.hyphenated.(hyphenStart_j) in (hyp,0,Array.length hyp)::lj0)
          | _->lj0)
     else lj0)
  in
    Printf.printf "%d %d\n" lineStart_j hyphenStart_j;
    (match paragraphs.(paragraph_j).(lineStart_j) with
         Hyphen x->Printf.printf "hyphen\n"
       | _->());
    flush stdout;
    match li, lj with
        (_,i,_)::_,(_,j,_)::_->
          collide li i lj j infinity
      | _->failwith "impossible case"

let collide paragraphs node0 node1 comp0 comp1 x0 x1=
  collide_ paragraphs
    node0.paragraph node0.lineStart node0.lineEnd node0.hyphenStart node0.hyphenEnd x0 comp0
    node1.paragraph node1.lineStart node1.lineEnd node1.hyphenStart node1.hyphenEnd x1 comp1

let _=
  let text=Parser.main (Dyp.from_string (Parser.pp ()) test) in
  let parsed=fst (List.hd text) in
  let pages=Array.of_list (List.map Array.of_list parsed) in
  let node={ paragraph=0; lineStart=0; lineEnd=19; lastFigure= -1; height=0; paragraph_height=0;
             hyphenStart= -1; hyphenEnd= 0; page=0 } in
  let node'={ paragraph=0; lineStart=19; lineEnd=52; lastFigure= -1; height=0; paragraph_height=0;
             hyphenStart= 0; hyphenEnd= -1; page=0 } in
  let comp=1. in
  let comp'=0. in
  let x0=0. in
  let x0'=0. in

    Graphics.open_graph "";
    Printf.printf "%d\n" (Array.length pages.(0));
    draw_line (round (x0*.mm),100) pages node comp;
    draw_line (round (x0'*.mm),20) pages node' comp';
    collide pages node node' comp comp' x0 x0';
    (* let col=collide pages 0 comp node.lineEnd 0 comp' node'.lineEnd *)
    (*   node.lineStart x0 *)
    (*   node'.lineStart x0' infinity *)
    (* in *)
    (*   Printf.printf "collision : %f\n" col; *)
    (*   flush stdout; *)
