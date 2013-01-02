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
open CamomileLibrary
open Typography
open Typography.Util
open Typography.Fonts
open Typography.Fonts.FTypes
open Typography.OutputCommon
open Typography.OutputPaper

module ClassMap=Map.Make(struct type t=int*float*OutputCommon.color let compare=compare end)

type font_cache={
  subfonts:(Fonts.font * (FTypes.glyph_id*int) IntMap.t) StrMap.t;
  fontBuffers:Rbuffer.t StrMap.t;
  mutable instances:(int StrMap.t) StrMap.t;
  fontFamilies:int StrMap.t;
  mutable classes:int ClassMap.t
}
let rand=ref 0

let build_font_cache prefix pages=
  Random.self_init ();
  rand:=(Random.int 0xffff);
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
        make_fonts i s (make_fonts (-2) l.link_contents fonts)
      | States st::s->
        make_fonts i s (make_fonts (-2) st.states_contents fonts)
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

  let style_buf=Rbuffer.create 256 in
  Rbuffer.add_string style_buf "body{line-height:0;}\n.z { font-size:0; }\n";
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

      let full=Printf.sprintf "%s_%d_%d_%d" (Fonts.fontName font).postscript_name instance subfont !rand in
      let info=Fonts.fontInfo font in
      let filename=Filename.concat prefix (full^".otf") in
      families:=StrMap.add (full) (StrMap.cardinal !families) !families;
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
      Rbuffer.output_buffer out buf;
      close_out out;
  ) cache.fontBuffers



(* renvoit (nom complet de la police, nom de la classe) *)
let className cache gl_=
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

  let full=Printf.sprintf "%s_%d_%d_%d" (Fonts.fontName font).postscript_name instance subfont !rand in
  (* let full=(Fonts.fontName font).postscript_name^"_"^(string_of_int instance)^"_"^(string_of_int subfont) in *)
  let fam=StrMap.find full cache.fontFamilies in

  try
    ClassMap.find (fam,gl_.glyph_size,gl_.glyph_color) cache.classes
  with
      Not_found->(
        let n=ClassMap.cardinal cache.classes in
        cache.classes<-ClassMap.add (fam,gl_.glyph_size,gl_.glyph_color) n cache.classes;
        n
      )


let make_style cache=
  let style_buf=Rbuffer.create 256 in
  StrMap.iter (fun full class_name->
    Rbuffer.add_string style_buf (Printf.sprintf "@font-face { font-family:f%d;
src:url(\"%s.otf\") format(\"opentype\"); }
.f%d { font-family:f%d; }\n"
                                    class_name full class_name class_name)
  ) cache.fontFamilies;
  style_buf
