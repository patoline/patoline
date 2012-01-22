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

let default={ format=a4; lead=3.; measure=150.; line_height=30 }


let test=
  "Ceci est un petit texte sympa, avec un premier paragraphe assez long "^
  "pour avoir des sauts de lignes : avec même des accents, surtout l'été !"

let mm=10.


let draw_glyph (x0,y0) gl=
  let fact=mm*.gl.size/.1000. in
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
    for i=node.lineStart to node.lineEnd-1 do
      match pages.(node.paragraph).(i) with
          GlyphBox gl->(
            Graphics.draw_rect
              (round !x) (!y+round (mm*.gl.y0*.gl.size/.1000.))
              (round (mm*.gl.width*.gl.size/.1000.)) (round (mm*.(gl.y1-.gl.y0)*.gl.size/.1000.));
            
            draw_glyph (round !x, !y) gl;
            
            x:= !x +. (mm*.gl.width*.gl.size/.1000.)
          )

        | Glue (a,_,c)->x:= !x +. (mm*.(a+.(c-.a)*.comp))
    done;
    flush stdout
    

let inf_zero x=if x=infinity || x= -.infinity then 0. else x


let rec collide lines pi comp_i max_i pj comp_j max_j i xi j xj max_col=
  if (i>=max_i && j>=max_j) then
    max_col
  else
    let wi=if i<max_i then box_width comp_i lines.(pi).(i) else 0. in
    let wj=if j<max_j then box_width comp_j lines.(pj).(j) else 0. in
      if xi +.wi < xj+. wj || j>=max_j then (
        let yi=inf_zero (if i>=max_i then 0. else lower_y lines.(pi).(i)) in
        let yj=inf_zero (if xi+.wi<xj || j>=max_j then 0. else upper_y lines.(pj).(j)) in
        let x0=if xi+.wi<xj then xi else max xi xj in
        let w0=xi +. wi -. x0 in
          if w0>=0. then (
            Graphics.set_color Graphics.red;
            Graphics.draw_rect (round (mm*.x0)) (round (20.+.mm*.yj)) (round (mm*.w0)) (round (80.+.mm*.(yi-.yj)));
            Graphics.set_color Graphics.black;
            (* let _=Graphics.wait_next_event [Graphics.Key_pressed] in *)
          );
          collide lines pi comp_i max_i pj comp_j max_j (i+1) (xi+.wi) j xj max_col

      ) else (
        let yi=inf_zero (if xj>xi+.wi || i>=max_i then 0. else lower_y lines.(pi).(i)) in
        let yj=inf_zero (if j>=max_j then 0. else upper_y lines.(pj).(j)) in
        let x0=if xj+.wj<xi then xj else max xi xj in
        let w0=xj +. wj -. x0 in
          if w0>=0. then (
            Graphics.set_color Graphics.green;
            Printf.printf "%d %d %d %d\n"(round (mm*.x0)) (round (200.+.mm*.yj)) (round (mm*.w0)) (round (80.+.mm*.(yi-.yj)));
            Graphics.draw_rect (round (mm*.x0)) (round (20.+.mm*.yj)) (round (mm*.w0)) (round (80.+.mm*.(yi-.yj)));
            Graphics.set_color Graphics.black;
            (* let _=Graphics.wait_next_event [Graphics.Key_pressed] in *)
          );
          collide lines pi comp_i max_i pj comp_j max_j i xi (j+1) (xj+.wj) max_col
      )


      
let _=
  let text=Parser.main (Dyp.from_string (Parser.pp ()) test) in
  let parsed=fst (List.hd text) in
  let pages=array_of_rev_list (List.map (array_of_rev_list) parsed) in
  let node={ paragraph=0; lineStart=0; lineEnd=68; lastFigure= -1; height=0; paragraph_height=0 } in
  let node'={ paragraph=0; lineStart=69; lineEnd=140; lastFigure= -1; height=0; paragraph_height=0 } in
  let comp=1. in
  let comp'=0. in
  let x0=10. in
  let x0'=0. in

    Graphics.open_graph "";
    draw_line (round (x0*.mm),100) pages node comp;
    draw_line (round (x0'*.mm),20) pages node' comp';
    let col=collide pages 0 comp node.lineEnd 0 comp' node'.lineEnd
      node.lineStart x0
      node'.lineStart x0' infinity
    in 
      Printf.printf "collision : %f\n" col; 
      flush stdout;
      let _=Graphics.wait_next_event [Graphics.Key_pressed] in
        ()


      
