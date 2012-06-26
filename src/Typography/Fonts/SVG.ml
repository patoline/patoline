open Fonts.FTypes

exception Bezier_degree

let pathbuf=Buffer.create 100

module SVG=functor (M:Font)->struct

  let output_glyph buf glyph=
    List.iter (function
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
                               ) (List.rev (h::s));
                     Printf.bprintf buf "Z"
                   )) (M.outlines glyph)
end


let f=Fonts.loadFont "/Users/pe/Projets/patoline/Fonts/Euler/euler.otf"
module S=SVG(Fonts)
let _=
  let c=Fonts.glyph_of_char f 'a' in
  let g=(Fonts.loadGlyph f { glyph_utf8="a";glyph_index=c }) in
  let buf=Buffer.create 100 in
  Printf.bprintf buf "<?xml version=\"1.0\" standalone=\"no\"?>\n";
  Printf.bprintf buf "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\" >\n";

  (* Printf.bprintf buf "<svg width=\"400px\" height=\"300px\" xmlns=\"http://www.w3.org/2000/svg\">\n"; *)
  Printf.bprintf buf "<svg xmlns=\"http://www.w3.org/2000/svg\">\n";

  Printf.bprintf buf "<defs>\n<font id=\"%s\" horiz-adv-x=\"%g\">\n" (Fonts.fontName f) 0.;
  Printf.bprintf buf "<font-face font-family=\"%s\"/>\n" (Fonts.fontName f);

  S.output_glyph pathbuf g;
  Printf.bprintf buf "<glyph glyph-name=\"%s\" unicode=\"%s\" horiz-adv-x=\"%g\" d=\"%s\"/>\n"
    (Digest.to_hex (Digest.string (Fonts.glyphContents g)))
    (Fonts.glyphContents g)
    (Fonts.glyphWidth g)
    (Buffer.contents pathbuf);

  (* Printf.bprintf buf "</font></defs>\n"; *)
  Printf.bprintf buf "</font></defs></svg>\n";

  (* Printf.bprintf buf "<text x=\"100\" y=\"100\" style=\"font-family: '%s'\">a</text></svg>\n" (Fonts.fontName f); *)
  Printf.printf "%s\n" (Buffer.contents buf)
