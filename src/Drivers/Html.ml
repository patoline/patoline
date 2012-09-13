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
  (* m : (font * (((glyph*int) IntMap.t) IntMap.t)) StrMap.t *)
  (* Pour chaque police, on associe au premier caractère c du glyphe
     la map de tous les glyphs qui commencent par c vers le numéro de
     sous-police où on doit stocker ce glyphe. *)

  let rec make_fonts i l fonts=
    match l with
        []->(
          if i+1>=Array.length pages then fonts else (
            make_fonts (i+1) (pages.(i+1).pageContents) fonts
          )
        )
      | Glyph h::s->(
        let font=Fonts.glyphFont h.glyph in
        let fontName=(Fonts.fontName font).postscript_name in
        let _,fontDict=
          try StrMap.find fontName fonts with
              Not_found->font,IntMap.empty
        in
        let num=Fonts.glyphNumber h.glyph in
        let c=UChar.code (UTF8.look num.glyph_utf8 0) in
          (* map de tous les caractères commençant par c *)
        let beginning_with_c=try IntMap.find c fontDict with Not_found->IntMap.empty in
        let beginning_with_c'=
          if IntMap.mem num.glyph_index beginning_with_c then
            beginning_with_c
          else
            IntMap.add num.glyph_index (num, IntMap.cardinal beginning_with_c)
              beginning_with_c
        in
        let fonts'=StrMap.add fontName (font,(IntMap.add c (beginning_with_c') fontDict)) fonts in
        make_fonts i s fonts'
      )
      | _::s->make_fonts i s fonts
  in
  let f=make_fonts (-1) [] StrMap.empty in
  (* Il faut fusionner les maps de tous les glyphes utilisés, pour ne
     plus les distinguer par premier caractère *)
  let f=StrMap.map (fun (font,a)->
    (font,
     IntMap.fold (fun char glyphMap m->
       IntMap.fold (fun glyphIdx (gl,subfont) n->
         IntMap.add glyphIdx (gl,subfont) n
       ) glyphMap m) a IntMap.empty)
  ) f
  in
  let style_buf=Rbuffer.create 256 in
  Rbuffer.add_string style_buf ".z { font-size:0; }\n";
  let classes=ref StrMap.empty in
  StrMap.iter (fun name (font,a)->
    (* k : nom de la police
       a : (glyph*int) IntMap.t : map du numéro de glyph vers la sous-police *)
    let sub_fonts=
      IntMap.fold (fun glyphIdx (gl,subfont) n->
        let glyphs=try IntMap.find subfont n with Not_found->[] in
        IntMap.add subfont (gl::glyphs) n
      ) a IntMap.empty
    in
    IntMap.iter (fun subfont glyphList->
      let glyphList=
        ({glyph_index=0;glyph_utf8="" })::glyphList
      in
      let glyphs=Array.of_list glyphList in
      let family=(Fonts.fontName font).family_name in
      let subfamily=(Fonts.fontName font).subfamily_name in
      let full=(Fonts.fontName font).postscript_name^"_"^(string_of_int subfont) in
      let ps=(Fonts.fontName font).postscript_name^"_"^(string_of_int subfont) in
      let info=Fonts.fontInfo font in
      let filename=full^".otf" in
      Fonts.setName info
        { family_name=family;
          subfamily_name=subfamily;
          full_name=full;
          postscript_name=ps };
      let class_name=Printf.sprintf "c%d" (StrMap.cardinal !classes) in
      classes:=StrMap.add ps class_name !classes;
      Rbuffer.add_string style_buf (Printf.sprintf "@font-face { font-family:%s;
src:url(\"%s\") format(\"opentype\"); }
.%s { font-family:%s; }\n"
                                      class_name filename class_name class_name);
      let bindings=ref IntMap.empty in
      let rec make_bindings i b=
        if i>=Array.length glyphs then b else (
          let gl=glyphs.(i) in
          make_bindings (i+1)
            (if UTF8.out_of_range gl.glyph_utf8 0 then b else
                IntMap.add (UChar.code (UTF8.look gl.glyph_utf8 0)) i b)
        )
      in
      let buf=Fonts.subset font info (make_bindings 1 IntMap.empty) glyphs in
      let out=open_out filename in
      Rbuffer.output_buffer out buf;
      close_out out;
    ) sub_fonts
  ) f;

  for i=0 to Array.length pages-1 do
    let html_name= ((Filename.chop_extension fileName)^(Printf.sprintf "%d" i)^".html") in
    let o=open_out html_name in
    Printf.fprintf o
      "<!DOCTYPE html>
<html lang=\"en\">
<head>
<meta charset=utf-8>
<title>%s</title>
<style type=\"text/css\">" structure.name;
    Rbuffer.output_buffer o style_buf;
    Printf.fprintf o "</style></head><body>\n";

    let w,h=pages.(i).pageFormat in
    List.iter (fun l->match l with
        Glyph x->(
          let font=Fonts.glyphFont x.glyph in
          let ps=(Fonts.fontName font).postscript_name in
          let idx=Fonts.glyphNumber x.glyph in
          let _,subfont=IntMap.find idx.glyph_index (snd (StrMap.find ps f)) in

          let ps=ps^"_"^(string_of_int subfont) in
          let class_name=StrMap.find ps !classes in
          let cont=Fonts.glyphNumber x.glyph in
          let pos=UTF8.next cont.glyph_utf8 0 in
          Printf.fprintf o "<span class=\"%s\">%s</span>"
            class_name
            (String.sub cont.glyph_utf8 0 pos);
          if String.length cont.glyph_utf8>pos then (
            Printf.fprintf o "<span class=\"z\">%s</span>"
              (String.sub cont.glyph_utf8 pos (String.length cont.glyph_utf8-pos))
          );
        )
      | Path (args, l)->(
      )
      | _->()
    ) pages.(i).pageContents;


    Printf.fprintf o "</body></html>\n";
    close_out o;
  done;
  Printf.fprintf stderr "File %s written.\n" fileName;
  flush stderr
