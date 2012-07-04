open Fonts.FTypes

exception Bezier_degree

let pathbuf=Buffer.create 100

let output_glyph buf glyph=
  List.iter (fun l->match List.rev l with
                 []->()
               | h::s->(
                   let x0,y0=h in
                   Printf.bprintf buf "M%g %g" x0.(0) y0.(0);
                   List.iter (fun (x,y)->
                                if Array.length x=2 then Printf.bprintf buf "L" else
                                  if Array.length x=3 then Printf.bprintf buf "Q" else
                                    if Array.length x=4 then Printf.bprintf buf "C" else
                                      raise Bezier_degree;
                                for i=1 to Array.length x-1 do
                                  Printf.bprintf buf "%g %g " x.(i) y.(i)
                                done
                             ) ((h::s));
                   Printf.bprintf buf "Z"
                 )) (Fonts.outlines glyph)


let f=Fonts.loadFont "/Users/pe/Projets/patoline/Fonts/Alegreya/Alegreya-Regular.otf"

let embed f buf glyphs=
  let y0=List.fold_left (fun m y->min m (Fonts.glyph_y0 y)) 0. glyphs in
  let y1=List.fold_left (fun m y->max m (Fonts.glyph_y1 y)) 0. glyphs in
  Printf.bprintf buf "<defs>\n<font horiz-adv-x=\"%g\">\n" 1000.;
  Printf.bprintf buf "<font-face font-family=\"%s\" ascent=\"%d\" descent=\"%d\" alphabetic=\"0\"/>\n" ("Alegreya") (int_of_float y1) (int_of_float y0);
  Printf.bprintf buf "<missing-glyph horiz-adv-x=\"1024\" d=\"M128 0V1638H896V0H128zM256 128H768V1510H256V128z\"/>";

  List.iter (fun g->
               Buffer.clear pathbuf;
               output_glyph pathbuf g;
               Printf.bprintf buf "<glyph unicode=\"%s\" horiz-adv-x=\"%g\" d=\"%s\"/>\n"
                 (Fonts.glyphContents g)
                 (Fonts.glyphWidth g)
                 (Buffer.contents pathbuf)
            ) glyphs;
  Printf.bprintf buf "</font></defs>\n"


let _=

(*   let o=open_out "test.css" in *)
(*   Printf.fprintf o "@font-face {  *)
(*   font-family: %s;  *)
(*   src: url(font.svg#Alegreya-Regular) format(\"svg\"); *)
(* } *)
(* body{font-family:\"%s\";}" (Fonts.fontName f) (Fonts.fontName f); *)
(*   close_out o; *)


(*   let o=open_out "test.html" in *)
(*   Printf.fprintf o "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\"> *)
(* <html> *)
(* <head> *)
(* <link rel=\"stylesheet\" type=\"text/css\" href=\"test.css\"> *)
(* </head> *)
(* <body> *)
(* abcdfl *)
(* </body></html> *)
(* "; *)
(*   close_out o; *)

  let o=open_out "test.svg" in
  let w=640 and h=480 in
  Printf.fprintf o "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1 Tiny//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11-tiny.dtd\">
<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" id=\"svg-root\" width=\"%d\" height=\"%d\" viewBox=\"0 0 %d %d\" version=\"1.1\" baseProfile=\"tiny\">" w h w h;

  Printf.fprintf o "<title id=\"test-title\">%s</title>\n" "titre";
  Printf.fprintf o "<desc id=\"test-desc\">Basic test of embedded fonts using glyph outlines</desc>\n";
  Printf.fprintf o "<g id=\"test-body-content\">\n";
  let buf=Buffer.create 100 in
  let glyphs=List.map (fun x->Fonts.loadGlyph f {glyph_utf8=String.make 1 x;glyph_index=Fonts.glyph_of_char f x})
    ['a';'b';'c']
  in
  embed f buf glyphs;
  Printf.fprintf o "%s" (Buffer.contents buf);
  Printf.fprintf o "<g transform=\"translate(165, 220)\" font-family=\"%s\" font-size=\"60\" fill=\"black\" stroke=\"none\">
<text>abc</text>
</g>
</g>
</svg>
" ("Alegreya");
  close_out o;
