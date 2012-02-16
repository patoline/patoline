open Fonts.FTypes
open Fonts.Opentype
let f="AGaramondPro-Regular.otf"

let font=loadFont f

let off=100
let mult=2.

let round x=off+(int_of_float (x/.mult))

let x=read_lookup font 4


let _=
  let xx0=100 in
  let yy0=100 in
  let arr= [| 34;612 |] in
    Graphics.open_graph "";
    let rec show_glyphs i=
      Graphics.clear_graph ();
      let gl=loadGlyph font { empty_glyph with glyph_index=arr.(i mod (Array.length arr)) } in
      let out=outlines gl in
        List.iter (fun (x,y)->
                     if Array.length x = 2 then
                       (let (a,b)=x.(0),y.(0) in
                        let (c,d)=x.(1),y.(1) in
                          Graphics.moveto (xx0+round a) (yy0+round b);
                          Graphics.lineto (xx0+round c) (yy0+round d))
                     else
                       (let (a,b)=x.(0),y.(0) in
                        let (c,d)=x.(1),y.(1) in
                        let (e,f)=x.(2),y.(2) in
                        let (g,h)=x.(3),y.(3) in
                          Graphics.moveto (xx0+round a) (yy0+round b);
                          Graphics.curveto (xx0+round c, yy0+round d) (xx0+round e, yy0+round f) (xx0+round g, yy0+round h))
                  ) out;
        let _=Graphics.wait_next_event [Graphics.Key_pressed] in
          show_glyphs (i+1)
    in
      show_glyphs 0
