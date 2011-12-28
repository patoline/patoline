let f="AGaramondPro-Regular.otf"

let font=Fonts.loadFont f 0
    
let _=match font with
    Fonts.CFF _->print_string "CFF\n"
  | _->print_string "Other\n"

let off=100
let mult=1.

let round x=off+(int_of_float (x/.mult))

let _=
  let i=ref 34 in
    Graphics.open_graph "";
    while true do
      Graphics.clear_graph ();
      let gl=Fonts.loadGlyph font 0 !i in
      let out=Fonts.outlines gl in
        List.iter (fun vect0->
                     match vect0 with
                         Bezier.Bezier vect->
                           if Array.length vect = 2 then 
                             (let (a,b)=vect.(0) in
                              let (c,d)=vect.(1) in
                                Graphics.moveto (round a) (round b);
                                Graphics.lineto (round c) (round d))
                           else
                             (let (a,b)=vect.(0) in
                              let (c,d)=vect.(1) in
                              let (e,f)=vect.(2) in
                              let (g,h)=vect.(3) in
                                Graphics.moveto (round a) (round b);
                                Graphics.curveto (round c, round d) (round e, round f) (round g, round h))
                  ) out;
        let _=Graphics.wait_next_event [Graphics.Key_pressed] in 
        incr i
    done
