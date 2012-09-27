open Typography
open CamomileLibrary
open Fonts.FTypes
open OutputCommon
open OutputPaper
open Util
open HtmlFonts

exception Bezier_degree

let filename x= try (Filename.chop_extension x)^".html" with _ -> x^".html"


let coord x=3.*.x

let assemble style title svg=
  let svg_buf=Rbuffer.create 256 in
  Rbuffer.add_string svg_buf "<defs>";
  Rbuffer.add_string svg_buf "<style type=\"text/css\">\n<![CDATA[\n";
  Rbuffer.add_buffer svg_buf style;
  Rbuffer.add_string svg_buf "]]>\n</style>\n";
  Rbuffer.add_string svg_buf "</defs>";
  Rbuffer.add_string svg_buf "<title>";
  Rbuffer.add_string svg_buf title;
  Rbuffer.add_string svg_buf "</title>";
  Rbuffer.add_buffer svg_buf svg;
  svg_buf

let standalone w h style title svg=
  let svg_buf=Rbuffer.create 256 in
  Rbuffer.add_string svg_buf "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1 Tiny//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11-tiny.dtd\">
<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" id=\"svg-root\"";
  Rbuffer.add_string svg_buf (Printf.sprintf "viewBox=\"0 0 %d %d\"" (round (coord w)) (round (coord h)));
  Rbuffer.add_string svg_buf "version=\"1.1\" baseProfile=\"tiny\">";
  Rbuffer.add_buffer svg_buf (assemble style title svg);
  svg_buf


let draw ?fontCache w h contents=
  let svg_buf=Rbuffer.create 256 in
  let def_buf=Rbuffer.create 256 in

  let fontCache=match fontCache with
      None->build_font_cache [|contents|]
    | Some x->x
  in
  (* Une petite burocratie pour gérer les particularités d'html/svg/etc *)
  let escapes=
    IntMap.add (int_of_char '<') "&lt;"
      (IntMap.add (int_of_char '>') "&gt;" IntMap.empty)
  in
  let esc_buf=Rbuffer.create 2 in
  let html_escape x=
    Rbuffer.clear esc_buf;
    for i=0 to String.length x-1 do
      try
        Rbuffer.add_string esc_buf
          (IntMap.find (int_of_char x.[i]) escapes)
      with
          Not_found -> Rbuffer.add_char esc_buf x.[i]
    done;
    Rbuffer.contents esc_buf
  in
  (****)


  let buf=Rbuffer.create 100 in

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

  (* Version alternative avec opentype *)
  StrMap.iter (fun full class_name->
    Rbuffer.add_string def_buf "@font-face { font-family:";
    Rbuffer.add_string def_buf class_name;
    Rbuffer.add_string def_buf "; src:url(\"";
    Rbuffer.add_string def_buf full;
    Rbuffer.add_string def_buf ".otf\") format(\"opentype\"); }\n"
  ) fontCache.classes;


  (* Écriture du contenu à proprement parler *)

  let cur_x=ref 0. in
  let cur_y=ref 0. in
  let cur_family=ref "" in
  let cur_size=ref 0. in
  let cur_color=ref (RGB {red=0.;green=0.;blue=0.}) in
  let opened_text=ref false in

  List.iter (fun cont->match cont with
      Glyph x->(
        let _,fontName=className fontCache x.glyph in
        let size=x.glyph_size in
        if !cur_x<>x.glyph_x || !cur_y<>x.glyph_y || !cur_family<>fontName
          || !cur_size<>size || !cur_color<>x.glyph_color || not !opened_text
        then (
          if !opened_text then (
            Rbuffer.add_string svg_buf "</text>";
          );
          Rbuffer.add_string svg_buf (Printf.sprintf "<text x=\"%g\" y=\"%g\" style=\"font-family:%s;font-size:%gpx;\" "
                                        (coord x.glyph_x) (coord (h-.x.glyph_y))
                                        fontName
                                        (coord size));
          (match x.glyph_color with
              RGB fc ->
                Rbuffer.add_string svg_buf
                  (Printf.sprintf "fill=\"#%02x%02x%02x\" "
                     (round (255.*.fc.red))
                     (round (255.*.fc.green))
                     (round (255.*.fc.blue)))
        (* | _->() *)
          );
          Rbuffer.add_string svg_buf "stroke=\"none\">";
          cur_x:=x.glyph_x;
          cur_y:=x.glyph_y;
          cur_family:=fontName;
          cur_size:=size;
          cur_color:=x.glyph_color;
          opened_text:=true;
        );
        let utf8=(Fonts.glyphNumber x.glyph).glyph_utf8 in
        Rbuffer.add_string svg_buf (html_escape (UTF8.init 1 (fun _->UTF8.look utf8 0)));
        cur_x:= !cur_x +. (Fonts.glyphWidth x.glyph)*.x.glyph_size/.1000.;
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
      )
    | Path (args, l)->(
      if !opened_text then (
        Rbuffer.add_string svg_buf "</text>";
      );
      Rbuffer.clear buf;
      List.iter
        (fun a->
          let x0,y0=a.(0) in
          Rbuffer.add_string buf (Printf.sprintf "M%g %g" (coord x0.(0)) (coord (h-.y0.(0))));
          Array.iter
            (fun (x,y)->
              if Array.length x=2 then Rbuffer.add_string buf "L" else
                if Array.length x=3 then Rbuffer.add_string buf "Q" else
                  if Array.length x=4 then Rbuffer.add_string buf "C" else
                    raise Bezier_degree;
              for j=1 to Array.length x-1 do
                Rbuffer.add_string buf (Printf.sprintf "%g %g " (coord x.(j)) (coord (h-.y.(j))));
              done
            ) a;
          if args.close then Rbuffer.add_string buf "Z"
        ) l;
      Rbuffer.add_string svg_buf "<path ";
      (match args.fillColor with
          Some (RGB fc) ->
            Rbuffer.add_string svg_buf (
              Printf.sprintf "fill=\"#%02X%02X%02X\" "
                (round (255.*.fc.red))
                (round (255.*.fc.green))
                (round (255.*.fc.blue))
            );
        | None->Rbuffer.add_string svg_buf "fill=\"none\" ");
      (match args.strokingColor with
          Some (RGB fc) ->
            Rbuffer.add_string svg_buf (
              Printf.sprintf "stroke=\"#%02X%02X%02X\" stroke-width=\"%f\" "
                (round (255.*.fc.red))
                (round (255.*.fc.green))
                (round (255.*.fc.blue))
                (coord args.lineWidth)
            );
        | None->
          Rbuffer.add_string svg_buf "stroke=\"none\" "
      );
      Rbuffer.add_string svg_buf "d=\"";
      Rbuffer.add_buffer svg_buf buf;
      Rbuffer.add_string svg_buf "\" />\n";
    )
    | _->()
      ) contents;
  if !opened_text then (
    Rbuffer.add_string svg_buf "</text>";
  );
  def_buf,svg_buf



let output ?(structure:structure={name="";displayname=[];
				  page= -1;struct_x=0.;struct_y=0.;substructures=[||]})
    pages fileName=

  let fileName = filename fileName in

  (* Il y a un truc de polices svg mutualisables entre les fichiers,
     mais ça ne marche que sur certaines versions de certains
     navigateurs. Quand un navigateur marchera, on pourra décommenter
     ce qui suit (si on le comprend encore). *)

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
  let cache=build_font_cache (Array.map (fun x->x.pageContents) pages) in

  for i=0 to Array.length pages-1 do
    let chop=Filename.chop_extension fileName in
    let chop_file=Filename.basename chop in
    let html_name=Printf.sprintf "%s%d.html" chop i in
    let w,h=pages.(i).pageFormat in
    let html=open_out html_name in
    let noscript=false in
    Printf.fprintf html
      "<!DOCTYPE html>
<html lang=\"en\">
<head>
<meta charset=\"utf-8\">
<title>%s</title>"      structure.name;
    if not noscript then
      Printf.fprintf html "<script>
resize=function(){
sizex=(window.innerWidth)/%g;
sizey=(window.innerHeight)/%g;
size=sizex>sizey ? sizey : sizex;
svg=document.getElementById(\"svg\");
svg.style.width=(%g*size)+'px';
svg.style.height=(%g*size)+'px';
};
//window.onresize=function(e){resize()};
window.onload=function(){resize()};
window.onkeydown=function(e){
%s
%s
}
</script>"
      w h (w-.10.) (h-.10.)
        (if i>0 then
            Printf.sprintf "if(e.keyCode==37){document.location.href=\"%s%d.html\"} // left" chop_file (i-1)
         else "")
        (if i<Array.length pages-1 then
            Printf.sprintf "if(e.keyCode==39){document.location.href=\"%s%d.html\"} //right" chop_file (i+1)
         else "");

    Printf.fprintf html "</head><body style=\"margin:0;padding:0;\">";
    if noscript then (
      Printf.fprintf html "<div style=\"margin:0;padding:0;\">%s%s%s</div>"
        (if i=0 then "" else
            Printf.sprintf "<a href=\"%s\">Précédent</a>"
              (Printf.sprintf "%s%d.html" chop_file (i-1)))
        (if i<>0 && i<>Array.length pages-1 then " " else "")
        (if i=Array.length pages-1 then "" else
            Printf.sprintf "<a href=\"%s\">Suivant</a>"
              (Printf.sprintf "%s%d.html" chop_file (i+1)));
    );
    Printf.fprintf html "<div id=\"svg\" style=\"margin-top:auto;margin-bottom:auto;margin-left:auto;margin-right:auto;\">";
    Printf.fprintf html "<svg  viewBox=\"0 0 %d %d\">"
      (round (coord w)) (round (coord h));
    let defs,svg=draw ~fontCache:cache w h pages.(i).pageContents in
    Rbuffer.output_buffer html (assemble defs "" svg);
    Printf.fprintf html "</svg>\n";
    Printf.fprintf html "</div></body></html>";
    close_out html
  done;
  Printf.fprintf stderr "File %s written.\n" fileName;
  flush stderr



let output' w h groups states transitions file=
  let pages=ref [] in
  let rec make_pages t=match t with
      L x->pages:=
        { pageFormat=w,h;pageContents=
            (List.concat (List.map (fun x->IntMap.find x groups) x))
        } ::(!pages)
    | N x->IntMap.iter (fun _ y->make_pages y) x
  in
  make_pages states;
  output (Array.of_list !pages) file
