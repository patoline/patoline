open Typography
open CamomileLibrary
open Fonts.FTypes
open OutputCommon
open OutputPaper
open Util


exception Bezier_degree

let filename x= try (Filename.chop_extension x)^".html" with _ -> x^".html"


let output ?(structure:structure={name="";displayname=[];
				  page= -1;struct_x=0.;struct_y=0.;substructures=[||]})
    pages fileName=

  let fileName = filename fileName in

  let pathbuf=Buffer.create 100 in

  let escapes=
    IntMap.add (int_of_char '<') "&lt;"
      (IntMap.add (int_of_char '>') "&gt;" IntMap.empty)
  in
  let esc_buf=Buffer.create 2 in
  let html_escape x=
    Buffer.clear esc_buf;
    for i=0 to String.length x-1 do
      try
        Buffer.add_string esc_buf
          (IntMap.find (int_of_char x.[i]) escapes)
      with
          Not_found -> Buffer.add_char esc_buf x.[i]
    done;
    Buffer.contents esc_buf
  in
  let fontRefs=ref StrMap.empty in

  let embed f buf glyphs=
    let fontNum=try StrMap.find f !fontRefs with Not_found -> (let c=StrMap.cardinal !fontRefs in
                                                               fontRefs:=StrMap.add f c !fontRefs;c)
    in
    let y0=List.fold_left (fun m (_,y)->min m (Fonts.glyph_y0 y)) 0. glyphs in
    let y1=List.fold_left (fun m (_,y)->max m (Fonts.glyph_y1 y)) 0. glyphs in
    Printf.bprintf buf "<font id=\"%s\" horiz-adv-x=\"%g\">\n" f 1000.;
    Printf.bprintf buf "<font-face font-family=\"%s\" ascent=\"%d\" descent=\"%d\" alphabetic=\"0\"/>\n" f (int_of_float y1) (int_of_float y0);
    Printf.bprintf buf "<missing-glyph horiz-adv-x=\"1024\" d=\"M128 0V1638H896V0H128zM256 128H768V1510H256V128z\"/>";
    let output_glyph buf glyph=
      List.iter (fun l->match l with
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
    in

    let variants=ref StrMap.empty in

    List.iter (fun (i,g)->
                 Buffer.clear pathbuf;
                 output_glyph pathbuf g;
                 let cont=Fonts.glyphContents g in
                 let variant=try StrMap.find cont !variants with Not_found->0 in
                 variants:=StrMap.add cont (variant+1) !variants;
                 Printf.bprintf buf "<glyph id=\"gl%d_%d\" %shoriz-adv-x=\"%g\" d=\"%s\"/>\n"
                   fontNum
                   i
                   (html_escape (if variant=0 then Printf.sprintf "unicode=\"%s\" " cont else ""))
                   (Fonts.glyphWidth g)
                   (Buffer.contents pathbuf);
              ) glyphs;

    Printf.bprintf buf "</font>\n";
    variants:=StrMap.empty;
    let actual_var=ref IntMap.empty in
    List.iter (fun (i,g)->
                 let cont=Fonts.glyphContents g in
                 let variant=try StrMap.find cont !variants with Not_found->0 in
                 variants:=StrMap.add cont (variant+1) !variants;
                 if variant>0 then (
                   actual_var:=IntMap.add i (Printf.sprintf "alt%d_%d" fontNum i) !actual_var;
                   Printf.bprintf buf "<altGlyphDef id=\"alt%d_%d\"><glyphRef xlink:href=\"#gl%d_%d\"/></altGlyphDef>\n" fontNum i fontNum i
                 )
              ) glyphs;
    !actual_var
  in

  let buf=Buffer.create 100 in
(*   (\* Collection des polices *\) *)
(*   let rec collect_fonts page fonts= *)
(*     if page>=Array.length pages then fonts else ( *)
(*       let fonts'=List.fold_left *)
(*         (fun m x->match x with *)
(*              Glyph gl->( *)
(*                let fname=Fonts.fontName (Fonts.glyphFont gl.glyph) in *)
(*                let f=try StrMap.find fname m with _->IntMap.empty in *)
(*                StrMap.add fname *)
(*                  (IntMap.add (Fonts.glyphNumber gl.glyph).glyph_index gl.glyph f) *)
(*                  m *)
(*              ) *)
(*            | _->m *)
(*         ) fonts pages.(page).pageContents *)
(*       in *)
(*       collect_fonts (page+1) fonts' *)
(*     ) *)
(*   in *)
(*   let doc_fonts=collect_fonts 0 StrMap.empty in *)
(*   let fontsName=Filename.chop_extension fileName ^ ".fonts.svg" in *)
(*   let o=open_out fontsName in *)
(*   Printf.fprintf o "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?> *)
(* <!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1 Tiny//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11-tiny.dtd\"> *)
(* <svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" id=\"svg-root\" version=\"1.1\" baseProfile=\"tiny\"><defs>"; *)

(*   let embedded_fonts=StrMap.mapi *)
(*     (fun k a-> *)
(*        let glyphs=IntMap.bindings a in *)
(*        Buffer.clear buf; *)
(*        let vars=embed k buf glyphs in *)
(*        Printf.fprintf o "%s" (Buffer.contents buf); *)
(*        vars *)
(*     ) doc_fonts *)
(*   in *)
(*   Printf.fprintf o "</defs></svg>\n"; *)
(*   close_out o; *)



  let coord x=x*.3. in
  let html=open_out fileName in
  Printf.fprintf html
    "<!DOCTYPE html>
<!DOCTYPE html>
<html lang=\"en\">
<head>
<meta charset=utf-8>
<title>%s</title><script>
current=0;
prev=function(){
if(current>0) {
current--;
document.getElementById(\"svg\").src=\"%s\"+current+\".svg\"
}
}
next=function(){
if(current<%d)
{
current++;
document.getElementById(\"svg\").src=\"%s\"+current+\".svg\"
}
}
window.onload=function(){
document.body.onkeydown=function(e){
if(e.which==37) prev()
else if(e.which==39) next();
}
}
</script>
</head><body>
<div><a href=\"#\" onclick=\"prev()\">Précédent</a> <a href=\"#\" onclick=\"next()\">Suivant</a></div>
<div><img style=\"border:1px solid black\" id=\"svg\" src=\"%s0.svg\"/></div>
</body></html>\n"
    structure.name
    (Filename.chop_extension fileName)
    (Array.length pages-1)
    (Filename.chop_extension fileName)
    (Filename.chop_extension fileName);

  for i=0 to Array.length pages-1 do
    let svg_name= ((Filename.chop_extension fileName)^(Printf.sprintf "%d" i)^".svg") in
    let o=open_out svg_name in
    let w,h=pages.(i).pageFormat in
    Printf.fprintf o "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1 Tiny//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11-tiny.dtd\">
<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" id=\"svg-root\" width=\"%d\" height=\"%d\" viewBox=\"0 0 %d %d\" version=\"1.1\" baseProfile=\"tiny\">" (round (coord w)) (round (coord h)) (round (coord w)) (round (coord h));

    Printf.fprintf o "<defs>\n";
    close_out html;

    let page_fonts=List.fold_left
      (fun m x->match x with
           Glyph gl->(
             let fname=Fonts.fontName (Fonts.glyphFont gl.glyph) in
             let f=try StrMap.find fname m with _->IntMap.empty in
             StrMap.add fname
               (IntMap.add (Fonts.glyphNumber gl.glyph).glyph_index gl.glyph f)
               m
           )
         | _->m
      ) StrMap.empty pages.(i).pageContents
    in

    let embedded_fonts=StrMap.mapi
      (fun k a->
         let glyphs=IntMap.bindings a in
         Buffer.clear buf;
         let vars=embed k buf glyphs in
         Printf.fprintf o "%s" (Buffer.contents buf);
         vars
      ) page_fonts
    in

    Printf.fprintf o "</defs>\n";


    Printf.fprintf o "<title>%s</title>\n" "titre";
    List.iter (function
                   Glyph x->(
                     Printf.fprintf o "<g font-family=\"%s\" font-size=\"%d\" "
                       (Fonts.fontName (Fonts.glyphFont x.glyph))
                       (round (coord x.glyph_size));
                     (match x.glyph_color with
                         RGB fc ->
                           Printf.fprintf o "fill=\"#%02x%02x%02x\" "
                             (round (255.*.fc.red))
                             (round (255.*.fc.green))
                             (round (255.*.fc.blue))
                       (* | _->() *));
                     Printf.fprintf o "stroke=\"none\">";
                     Printf.fprintf o "<text x=\"%g\" y=\"%g\">"
                       (coord x.glyph_x) (coord (h-.x.glyph_y));
                     let ff=StrMap.find (Fonts.fontName (Fonts.glyphFont x.glyph)) embedded_fonts in
                     let fi=try IntMap.find ((Fonts.glyphNumber x.glyph).glyph_index) ff with Not_found->"" in
                     if fi="" then
                       Printf.fprintf o "%s" (html_escape (Fonts.glyphContents x.glyph))
                     else (
                       Printf.fprintf o "<altGlyph xlink:href=\"#alt%d_%d\">%s</altGlyph>"
                         (StrMap.find (Fonts.fontName (Fonts.glyphFont x.glyph)) !fontRefs)
                         ((Fonts.glyphNumber x.glyph).glyph_index)
                         (html_escape (Fonts.glyphContents x.glyph))
                     );
                     Printf.fprintf o "</text></g>"
                   )
                 | Path (args, l)->(
                     Buffer.clear buf;
                     List.iter
                       (fun a->
                          let x0,y0=a.(0) in
                          Printf.bprintf buf "M%g %g" (coord x0.(0)) (coord (h-.y0.(0)));
                          Array.iter
                            (fun (x,y)->
                               if Array.length x=2 then Printf.bprintf buf "L" else
                                 if Array.length x=3 then Printf.bprintf buf "Q" else
                                   if Array.length x=4 then Printf.bprintf buf "C" else
                                     raise Bezier_degree;
                               for j=1 to Array.length x-1 do
                                 Printf.bprintf buf "%g %g " (coord x.(j)) (coord (h-.y.(j)))
                               done
                            ) a;
                          if args.close then Printf.bprintf buf "Z"
                       ) l;
                     Printf.fprintf o
                       "<path ";
                     (match args.fillColor with
                          Some (RGB fc) ->
                            Printf.fprintf o "fill=\"#%02X%02X%02X\" "
                              (round (255.*.fc.red))
                              (round (255.*.fc.green))
                              (round (255.*.fc.blue));
                        | None->Printf.fprintf o "fill=\"none\" ");
                     (match args.strokingColor with
                          Some (RGB fc) ->
                            Printf.fprintf o "stroke=\"#%02X%02X%02X\" stroke-width=\"%f\" "
                              (round (255.*.fc.red))
                              (round (255.*.fc.green))
                              (round (255.*.fc.blue))
                              (coord args.lineWidth)
                        | None->
                            Printf.fprintf o "stroke=\"none\" "
                     );
                     Printf.fprintf o "d=\"%s\" />\n" (Buffer.contents buf);
                   )
                 | _->()
              ) pages.(i).pageContents;
    Printf.fprintf o "</svg>";
    close_out o;
  done;
  Printf.fprintf stderr "File %s written.\n" fileName;
  flush stderr
