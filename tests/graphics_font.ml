open Types
let f="AGaramondPro-Regular.otf"

let font=Fonts.loadFont f

let _=match font with
    Fonts.CFF _->print_string "CFF\n"
  | _->print_string "Other\n"

let off=100
let mult=2.

let round x=off+(int_of_float (x/.mult))




let _=
  let xx0=100 in
  let yy0=10 in
  let i=ref 233 in
    Graphics.open_graph "";
    while true do
      Graphics.clear_graph ();
      let gl=Fonts.loadGlyph font { empty_glyph with glyph_index= !i } in
      let out=Fonts.outlines gl in
      (* let (x0,y0),(x1,y1)=List.fold_left Bezier.larger ((1./.0.,1./.0.),(-1./.0.,-1./.0.)) (List.map Bezier.bounding_box out) in *)
        (* Graphics.moveto (round x0) (round y0); *)
        (* Graphics.lineto (round x0) (round y1); Graphics.lineto (round x1) (round y1); *)
        (* Graphics.lineto (round x1) (round y0); Graphics.lineto (round x0) (round y0); *)
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
        incr i
    done
