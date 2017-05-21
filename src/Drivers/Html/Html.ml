(*
  Copyright Florian Hatat, Tom Hirschowitz, Pierre Hyvernat,
  Pierre-Etienne Meunier, Christophe Raffalli, Guillaume Theyssier 2012.

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
open FTypes
open Util
open HtmlFonts
open Driver
open RawContent

let driver_options = []
let filter_options argv = argv

exception Bezier_degree

let filename x= try (Filename.chop_extension x)^".html" with _ -> x^".html"

let output ?(structure:structure=empty_structure) pages fileName =

  let fileName = filename fileName in
  (* m : (font * (((glyph*int) IntMap.t) IntMap.t)) StrMap.t *)
  (* Pour chaque police, on associe au premier caractère c du glyphe
     la map de tous les glyphs qui commencent par c vers le numéro de
     sous-police où on doit stocker ce glyphe. *)
  let cache=build_font_cache (Filename.dirname fileName) (Array.map (fun x->x.contents) pages) in

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

    let w,h=pages.(i).size in
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
    ) pages.(i).contents;

    Printf.fprintf o "</nobr>\n";
    Printf.fprintf o "</body></html>\n";
    close_out o;
  done;
  Printf.fprintf stderr "File %s written.\n" fileName;
  flush stderr

let output' = output_to_prime output

let _ =
  Hashtbl.add DynDriver.drivers "Html" (
    module struct
      let output  = output
      let output' = output'
    end : OutputDriver)
