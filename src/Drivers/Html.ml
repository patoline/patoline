open Typography
open CamomileLibrary
open Fonts.FTypes
open OutputCommon
open OutputPaper
open Util
open HtmlFonts

exception Bezier_degree

let filename x= try (Filename.chop_extension x)^".html" with _ -> x^".html"



let output ?(structure:structure={name="";displayname=[];metadata=[];tags=[];
				  page= -1;struct_x=0.;struct_y=0.;substructures=[||]})
    pages fileName=

  let fileName = filename fileName in
  (* m : (font * (((glyph*int) IntMap.t) IntMap.t)) StrMap.t *)
  (* Pour chaque police, on associe au premier caractère c du glyphe
     la map de tous les glyphs qui commencent par c vers le numéro de
     sous-police où on doit stocker ce glyphe. *)
  let cache=build_font_cache (Filename.dirname fileName) (Array.map (fun x->x.pageContents) pages) in

  for i=0 to Array.length pages-1 do
    let html_name= ((Filename.chop_extension fileName)^(Printf.sprintf "%d" i)^".html") in
    let o=open_out html_name in
    Printf.fprintf o
      "<!DOCTYPE html>
<html lang=\"en\">
<head>
<meta charset=utf-8>
<title>%s</title>
<style type=\"text/css\">
body{line-height:0;}\n" structure.name;
    Rbuffer.output_buffer o (make_style cache);
    Printf.fprintf o "</style></head><body>\n";

    let w,h=pages.(i).pageFormat in
    let cur_x=ref 0. in
    let cur_y=ref 0. in
    let cur_class=ref (-1) in
    let span_open=ref false in
    let close_span ()=if !span_open then (
      Printf.fprintf o "</span>"; span_open:=false
    )
    in
    Printf.fprintf o "<nobr>\n";
    List.iter (fun l->match l with
        Glyph x->(
          let class_name=className cache x in
          let cont=Fonts.glyphNumber x.glyph in
          let pos=UTF8.next cont.glyph_utf8 0 in
          if class_name<> !cur_class then (
              close_span ();
              span_open:=true;
              let pos_y=h-.x.glyph_y (* -. (Fonts.ascender font)*.x.glyph_size/.1000. *) in

              Printf.fprintf o "<span class=\"c%d\" style=\"position:absolute;left:%fmm;top:%fmm;\">%s"
                class_name
                x.glyph_x
                pos_y
                (String.sub cont.glyph_utf8 0 pos);
              cur_class:=class_name;
          ) else (
              Printf.fprintf o "%s" (String.sub cont.glyph_utf8 0 pos);
            );
          cur_x:= x.glyph_x +. (Fonts.glyphWidth x.glyph)*.x.glyph_size/.1000.;
          cur_y:= x.glyph_y;
          (* if String.length cont.glyph_utf8>pos then ( *)
          (*   Printf.fprintf o "<span class=\"z\">%s</span>" *)
          (*     (String.sub cont.glyph_utf8 pos (String.length cont.glyph_utf8-pos)) *)
          (* ); *)
        )
      | Path (args, l)->(
      )
      | _->()
    ) pages.(i).pageContents;

    Printf.fprintf o "</nobr>\n";
    Printf.fprintf o "</body></html>\n";
    close_out o;
  done;
  Printf.fprintf stderr "File %s written.\n" fileName;
  flush stderr
