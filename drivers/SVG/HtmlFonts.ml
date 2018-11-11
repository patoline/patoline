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

open Patutil
open Extra
open Patfonts
open Fonts
open FTypes
open Patoraw
open RawContent
open Unicodelib

module ClassMap = Map.Make(
  struct
    type t = int * float * Color.color
    let compare = compare
  end)

type font_cache={
  subfonts:(Fonts.font * (FTypes.glyph_id*int) IntMap.t) StrMap.t;
  fontBuffers:Buffer.t StrMap.t;
  mutable instances:(int StrMap.t) StrMap.t;
  fontFamilies:(string * int) StrMap.t;
  mutable classes:int ClassMap.t
}

let build_font_cache prefix pages=
  let rec make_fonts i l fonts=
    match l with
        []->(
          if i<(-1) || i+1>=Array.length pages then fonts else (
            make_fonts (i+1) (pages.(i+1)) fonts
          )
        )
      | Glyph h::s->(
        let font=Fonts.glyphFont h.glyph in
        let fontName=(Fonts.uniqueName font) in
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
      | Link l::s->
         let fonts =
           match l.link_kind with
           | Button(Menu(items), name) ->
              List.fold_left (fun fonts (_,c) -> make_fonts (-2) c fonts) fonts items
           | _ -> fonts
         in
         make_fonts i s (make_fonts (-2) l.link_contents fonts)
      | States st::s->
        make_fonts i s (make_fonts (-2) st.states_contents fonts)
      | Animation a::s ->
        make_fonts i s (Array.fold_left (fun fonts c -> make_fonts (-2) c fonts) fonts a.anim_contents)
      | (Video _ | Image _ | Path _)::s->make_fonts i s fonts
      | Dynamic d::s ->
        make_fonts i s (make_fonts (-2) (d.dyn_contents ()) (make_fonts (-2) d.dyn_sample fonts));
      | Affine a::s ->
        make_fonts i s (make_fonts (-2) a.affine_contents fonts)
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

  (* Eviter que deux fonts avec le même nom et des fichiers différents ne s'emmèlent *)
  let fontInstances=ref StrMap.empty in
  let fontInstance font=
    let u=Fonts.uniqueName font in
    let p=(Fonts.fontName font).postscript_name in
    let instances=try StrMap.find p !fontInstances with Not_found->StrMap.empty in
    let inst_num=try StrMap.find u instances with Not_found->StrMap.cardinal instances in
    fontInstances:=StrMap.add p (StrMap.add u inst_num instances) !fontInstances;
    inst_num
  in

  let style_buf=Buffer.create 256 in
  Buffer.add_string style_buf "body{line-height:0;}\n.z { font-size:0; }\n";
  let families=ref StrMap.empty in
  let fontBuffers=ref StrMap.empty in
  let classes=ref ClassMap.empty in
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
      let instance=fontInstance font in

      let info=Fonts.fontInfo font in
      let rec make_bindings i b=
        if i>=Array.length glyphs then b else (
          let gl=glyphs.(i) in
          make_bindings (i+1)
            (if UTF8.out_of_range gl.glyph_utf8 0 then b else
                IntMap.add (UChar.code (UTF8.look gl.glyph_utf8 0)) i b)
        )
      in
      Fonts.add_kerning info [];
      let buf=Fonts.subset font info (make_bindings 1 IntMap.empty) glyphs in
      let notFull = Printf.sprintf "%s_%d_%d_" (Fonts.fontName font).postscript_name instance subfont in
      let full = notFull ^ (Digest.to_hex (Digest.string (Buffer.contents buf))) in
      families:=StrMap.add (notFull) (full, StrMap.cardinal !families) !families;
      let filename=Filename.concat prefix (full^".otf") in
      fontBuffers:=StrMap.add filename buf !fontBuffers;
    ) sub_fonts
  ) f;
  { subfonts=f;
    fontBuffers= !fontBuffers;
    instances= !fontInstances;
    fontFamilies= !families;
    classes= !classes }


let output_fonts cache=
  StrMap.iter (fun filename buf->
      let out=open_out filename in
      Buffer.output_buffer out buf;
      close_out out;
  ) cache.fontBuffers

let filter_fonts cmd cache =
  let temp_dir = Filename.temp_file "patfonts" "" in
  Sys.remove temp_dir; Unix.mkdir temp_dir 0o700;
  StrMap.iter (fun filename buf->
    let filename = Filename.(concat temp_dir (basename filename)) in
    let out=open_out filename in
    Buffer.output_buffer out buf;
    close_out out;
    let cmd = cmd ^ " " ^ filename in
    Printf.eprintf "filtering: %s\n" cmd;
    if Sys.command cmd <> 0 then
      begin
        Printf.eprintf "font filter command %S failed\n" cmd;
        exit 1
      end;
    Buffer.clear buf;
    let cin =open_in filename in
    Buffer.add_channel buf cin (in_channel_length cin);
    close_in cin;
    Sys.remove filename;
  ) cache.fontBuffers;
  Unix.rmdir temp_dir

let classStyle cache gl_=
  let gl=gl_.glyph in
  let font=Fonts.glyphFont gl in
  let idx=Fonts.glyphNumber gl in
  let u=Fonts.uniqueName font in
  let _,subfont=IntMap.find idx.glyph_index (snd (StrMap.find u cache.subfonts)) in

  (* fontInstances *)
  let p=(Fonts.fontName font).postscript_name in
  let instances=try StrMap.find p cache.instances with Not_found->StrMap.empty in
  let inst_num=try StrMap.find u instances with Not_found->StrMap.cardinal instances in
  cache.instances<-StrMap.add p (StrMap.add u inst_num instances) cache.instances;
  let instance=inst_num in
  (*  *)

  let notFull=Printf.sprintf "%s_%d_%d_" (Fonts.fontName font).postscript_name instance subfont in
  (* let full=(Fonts.fontName font).postscript_name^"_"^(string_of_int instance)^"_"^(string_of_int subfont) in *)
  let full, fam=StrMap.find notFull cache.fontFamilies in
  (fam,gl_.glyph_size,gl_.glyph_color)

exception Style of (int * float * Color.color) * int

let max_old_class = ref (-1)

(* renvoit (nom complet de la police, nom de la classe) *)
let className ?(create_new_class=true) cache gl_=
  let style = classStyle cache gl_ in
  try
    let n = ClassMap.find style cache.classes in
    if n > !max_old_class then
      raise (Style (style, n))
    else n
  with
  | Not_found ->
     let n=ClassMap.cardinal cache.classes in
     cache.classes<-ClassMap.add style n cache.classes;
     if create_new_class then
       begin
         max_old_class := n; n
       end
     else
       raise (Style (style, n))





let make_style cache=
  let style_buf=Buffer.create 256 in
  StrMap.iter (fun _ (full, class_name) ->
    Buffer.add_string style_buf (Printf.sprintf "%@font-face { font-family:f%d;
src:url(\"%s.otf\") format(\"opentype\"); }
.f%d { font-family:f%d; }\n"
                                    class_name full class_name class_name)
  ) cache.fontFamilies;
  style_buf
