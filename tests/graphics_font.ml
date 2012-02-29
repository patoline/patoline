open Fonts.FTypes
open Fonts.Opentype
open Bezier
let f = try Sys.argv.(1) with _ -> "AGaramondPro-Regular.otf"
let initial_glyph = try int_of_string Sys.argv.(2) with _ -> 0

let font= loadFont f
let nb_glyphs = cardinal font
let round=Binary.round


let draw_glyph xx0 yy0 glyph=
  let _,miny,_,maxy, arr=glyph_roots glyph in
    for y=miny to miny + Array.length arr-1 do
      let rec draw=function
          x0::x1::s -> (
            let g0=round (255.*.(x0-.floor x0)) in
            let g1=round (255.*.(ceil x1-.x1)) in
              Graphics.set_color Graphics.black;
              Graphics.moveto (xx0+(int_of_float (ceil x0))) (yy0+y);
              Graphics.lineto (xx0+(int_of_float (floor x1))) (yy0+y);
              Graphics.set_color (Graphics.rgb g0 g0 g0);
              Graphics.plot (xx0+(int_of_float (floor x0))) (yy0+y);
              Graphics.set_color (Graphics.rgb g1 g1 g1);
              Graphics.plot (xx0+(int_of_float (ceil x1))) (yy0+y);
              draw s
          )
        | _-> ()
      in draw arr.(y-miny)
    done


let _=
  let xx0=100 in
  let yy0=100 in
  let arr = Array.init nb_glyphs (fun i -> i) in
    Graphics.open_graph "";
    Graphics.auto_synchronize false;
    let rec show_glyphs i=
      let gl=loadGlyph font { empty_glyph with glyph_index=arr.(i mod (Array.length arr)) } in
        Graphics.clear_graph ();

        Graphics.set_color (Graphics.rgb 150 150 150);
        Graphics.moveto 5 5; Graphics.draw_string "'n': +1  'N': +50  'p': -1 'P': -50  'q': quit";
        Graphics.moveto 10 20; Graphics.draw_string (f ^ ", glyph " ^ (string_of_int (glyphNumber gl).glyph_index) ^ " / "
                                                     ^ (string_of_int nb_glyphs) ^ " width : "^(string_of_float (glyphWidth gl)) );
        let out=outlines gl in
          draw_glyph xx0 yy0 out;
            Graphics.synchronize ();
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
