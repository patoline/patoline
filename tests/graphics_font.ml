open Fonts.FTypes
open Fonts.Opentype

let f = try Sys.argv.(1) with _ -> "AGaramondPro-Regular.otf"
let initial_glyph = try int_of_string Sys.argv.(2) with _ -> 0

let font= loadFont f
let nb_glyphs = cardinal font
let round x=(int_of_float (x/.2.))

let x=read_lookup font 4


let _=
  let xx0=100 in
  let yy0=100 in
  let arr = Array.init nb_glyphs (fun i -> i) in
    Graphics.open_graph "";
    let rec show_glyphs i=
      let gl=loadGlyph font { empty_glyph with glyph_index=arr.(i mod (Array.length arr)) } in
      Graphics.clear_graph ();

      Graphics.set_color (Graphics.rgb 150 150 150);
      Graphics.moveto 5 5; Graphics.draw_string "'n': +1  'N': +50  'p': -1 'P': -50  'q': quit";
      Graphics.moveto 10 20; Graphics.draw_string (f ^ ", glyph " ^ (string_of_int (glyphNumber gl).glyph_index) ^ " / " ^ (string_of_int nb_glyphs));
      Graphics.set_color Graphics.black;

      Graphics.moveto 0 yy0;Graphics.lineto (Graphics.size_x()) yy0;
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
        let s=Graphics.wait_next_event [Graphics.Key_pressed] in
          match s.Graphics.key with
             'q' -> exit 0
           | 'p' -> show_glyphs (Array.length arr + i-1)
           | 'P' -> show_glyphs (Array.length arr + i-50)
           | 'N' -> show_glyphs (Array.length arr + i+50)
           | ' ' | 'n' -> show_glyphs (i+1)
           | _ -> show_glyphs (i+1)
    in
      show_glyphs initial_glyph
