(*
  Copyright Tom Hirschowitz, Florian Hatat, Pierre-Etienne Meunier,
  Christophe Raffalli and others, 2012.

  This file is part of Patoline.

  Patoline is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Patoline is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Patoline.  If not, see <http://www.gnu.org/licenses/>.
*)

  (* Polices SVG intégrées (pas dans firefox aujourd'hui) *)
  (* let page_fonts=List.fold_left *)
  (*   (fun m x->match x with *)
  (*       Glyph gl->( *)
  (*         let fname=Fonts.fontName (Fonts.glyphFont gl.glyph) in *)
  (*         let f=try StrMap.find fname.full_name m with _->IntMap.empty in *)
  (*         StrMap.add fname.full_name *)
  (*           (IntMap.add (Fonts.glyphNumber gl.glyph).glyph_index gl.glyph f) *)
  (*           m *)
  (*       ) *)
  (*     | _->m *)
  (*   ) StrMap.empty contents *)
  (* in *)
  (* let fontRefs=ref StrMap.empty in *)

  (* let embed f buf glyphs= *)
  (*   let fontNum=try StrMap.find f !fontRefs with Not_found -> (let c=StrMap.cardinal !fontRefs in *)
  (*                                                              fontRefs:=StrMap.add f c !fontRefs;c) *)
  (*   in *)
  (*   let y0=List.fold_left (fun m (_,y)->min m (Fonts.glyph_y0 y)) 0. glyphs in *)
  (*   let y1=List.fold_left (fun m (_,y)->max m (Fonts.glyph_y1 y)) 0. glyphs in *)
  (*   Printf.bprintf buf "<font id=\"%s\" horiz-adv-x=\"%g\">\n" f 1000.; *)
  (*   Printf.bprintf buf "<font-face font-family=\"%s\" ascent=\"%d\" descent=\"%d\" alphabetic=\"0\"/>\n" f (int_of_float y1) (int_of_float y0); *)
  (*   Printf.bprintf buf "<missing-glyph horiz-adv-x=\"1024\" d=\"M128 0V1638H896V0H128zM256 128H768V1510H256V128z\"/>"; *)
  (*   let output_glyph buf glyph= *)
  (*     List.iter (fun l->match l with *)
  (*         []->() *)
  (*       | h::s->( *)
  (*         let x0,y0=h in *)
  (*         Printf.bprintf buf "M%g %g" x0.(0) y0.(0); *)
  (*         List.iter (fun (x,y)-> *)
  (*           if Array.length x=2 then Printf.bprintf buf "L" else *)
  (*             if Array.length x=3 then Printf.bprintf buf "Q" else *)
  (*               if Array.length x=4 then Printf.bprintf buf "C" else *)
  (*                 raise Bezier_degree; *)
  (*           for i=1 to Array.length x-1 do *)
  (*             Printf.bprintf buf "%g %g " x.(i) y.(i) *)
  (*           done *)
  (*         ) ((h::s)); *)
  (*         Printf.bprintf buf "Z" *)
  (*       )) (Fonts.outlines glyph) *)
  (*   in *)
  (*   let variants=ref StrMap.empty in *)
  (*   let pathbuf=Buffer.create 100 in *)
  (*   List.iter (fun (i,g)-> *)
  (*     Buffer.clear pathbuf; *)
  (*     output_glyph pathbuf g; *)
  (*     let cont=Fonts.glyphContents g in *)
  (*     let variant=try StrMap.find cont !variants with Not_found->0 in *)
  (*     variants:=StrMap.add cont (variant+1) !variants; *)
  (*     Printf.bprintf buf "<glyph id=\"gl%d_%d\" %shoriz-adv-x=\"%g\" d=\"%s\"/>\n" *)
  (*       fontNum *)
  (*       i *)
  (*       (html_escape (if variant=0 then Printf.sprintf "unicode=\"%s\" " cont else "")) *)
  (*       (Fonts.glyphWidth g) *)
  (*       (Buffer.contents pathbuf); *)
  (*   ) glyphs; *)
  (*   Printf.bprintf buf "</font>\n"; *)
  (*   variants:=StrMap.empty; *)
  (*   let actual_var=ref IntMap.empty in *)
  (*   List.iter (fun (i,g)-> *)
  (*     let cont=Fonts.glyphContents g in *)
  (*     let variant=try StrMap.find cont !variants with Not_found->0 in *)
  (*     variants:=StrMap.add cont (variant+1) !variants; *)
  (*     if variant>0 then ( *)
  (*       actual_var:=IntMap.add i (Printf.sprintf "alt%d_%d" fontNum i) !actual_var; *)
  (*       Printf.bprintf buf "<altGlyphDef id=\"alt%d_%d\"><glyphRef xlink:href=\"#gl%d_%d\"/></altGlyphDef>\n" fontNum i fontNum i *)
  (*     ) *)
  (*   ) glyphs; *)
  (*   !actual_var *)
  (* in *)

  (* let embedded_fonts=StrMap.mapi *)
  (*   (fun k a-> *)
  (*     let glyphs=IntMap.bindings a in *)
  (*     Buffer.clear buf; *)
  (*     let vars=embed k buf glyphs in *)
  (*     Printf.fprintf o "%s" (Buffer.contents buf); *)
  (*     vars *)
  (*   ) page_fonts *)
  (* in *)



        (* let ff=StrMap.find (Fonts.fontName (Fonts.glyphFont x.glyph)).full_name embedded_fonts in *)
        (* let fi=try IntMap.find ((Fonts.glyphNumber x.glyph).glyph_index) ff with Not_found->"" in *)
        (* if fi="" then *)
        (*   Printf.fprintf o "%s" (html_escape (Fonts.glyphContents x.glyph)) *)
        (* else ( *)
        (*   Printf.fprintf o "<altGlyph xlink:href=\"#alt%d_%d\">%s</altGlyph>" *)
        (*     (StrMap.find (Fonts.fontName (Fonts.glyphFont x.glyph)).full_name !fontRefs) *)
        (*     ((Fonts.glyphNumber x.glyph).glyph_index) *)
        (*     (html_escape (Fonts.glyphContents x.glyph)) *)
          (* ); *)
