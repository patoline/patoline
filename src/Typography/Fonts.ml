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
open Bezier
open FTypes
open CamomileLibrary


exception Not_supported

module FTypes=FTypes

module type Font=FTypes.Font
module Opentype=Opentype
module CFF=CFF

module Opentype_=(Opentype:Font)
module CFF_=(CFF:Font)

(** loadFont pretends it can recognize font file types, but it
    actually only looks at the extension in the file name *)
type font = CFF of CFF.font | Opentype of Opentype.font
type glyph = CFFGlyph of CFF.glyph | OpentypeGlyph of Opentype.glyph


let fontName  ?index:(index=0) f=
  match f with
      CFF x->CFF.fontName ~index:index x
    | Opentype x->Opentype.fontName ~index:index x

let uniqueName  ?index:(index=0) f=
  match f with
      CFF x->CFF.uniqueName x
    | Opentype x->Opentype.uniqueName x

let loadFont ?offset:(off=0) ?size:(_=0) f=
  let size=let i=Util.open_in_bin_cached f in in_channel_length i in
  let font=if Filename.check_suffix f ".otf" || Filename.check_suffix f ".ttf" then
      Opentype (Opentype.loadFont ~offset:off f ~size:size)
    else if Filename.check_suffix f ".cff" then
      CFF (CFF.loadFont ~offset:off f ~size:size)
    else
    raise Not_supported
  in
#ifdef BAN_COMIC_SANS
  let is_substring s1 s0 i0=
    let rec sub i j=
      if i>String.length s0-String.length s1 then -1 else
        if j>=String.length s1 then i else
          if s0.[i+j]=s1.[j] then sub i (j+1) else
            sub (i+1) 0
    in
    sub i0 0
  in
  let low=(String.lowercase (fontName font).full_name) in
  let comic=is_substring "comic" low 0 in
    if comic<0 then font else
      let sans=is_substring "sans" low comic in
      if sans<0 then font else (
        output_string stderr (TypoLanguage.message TypoLanguage.Ban_comic_sans);
        exit 1
      )
#else
    font
#endif

let glyph_of_uchar f c=
  match f with
      CFF x->CFF.glyph_of_uchar x c
    | Opentype x->Opentype.glyph_of_uchar x c
let glyph_of_char f c=glyph_of_uchar f (UChar.of_char c)


let loadGlyph f ?index:(index=0) g=
  match f with
      CFF x->CFFGlyph (CFF.loadGlyph x ~index:index g)
    | Opentype x->OpentypeGlyph (Opentype.loadGlyph x ~index:index g)

let cardinal f=
  match f with
      CFF x->CFF.cardinal x
    | Opentype x->Opentype.cardinal x

let ascender f=
  match f with
      CFF x->CFF.ascender x
    | Opentype x->Opentype.ascender x
let descender f=
  match f with
      CFF x->CFF.descender x
    | Opentype x->Opentype.descender x

let outlines gl=
  match gl with
      CFFGlyph x->CFF.outlines x
    | OpentypeGlyph x->Opentype.outlines x

let glyphFont gl=
  match gl with
      CFFGlyph x->CFF (CFF.glyphFont x)
    | OpentypeGlyph x->Opentype (Opentype.glyphFont x)

let glyphContents gl=
  match gl with
      CFFGlyph x->CFF.glyphContents x
    | OpentypeGlyph x->Opentype.glyphContents x


let glyphNumber gl=
  match gl with
      CFFGlyph x->CFF.glyphNumber x
    | OpentypeGlyph x->Opentype.glyphNumber x

let glyphWidth gl=
  match gl with
      CFFGlyph x->CFF.glyphWidth x
    | OpentypeGlyph x->Opentype.glyphWidth x

let glyph_y0 gl=
  match gl with
      CFFGlyph x->CFF.glyph_y0 x
    | OpentypeGlyph x->Opentype.glyph_y0 x

let glyph_y1 gl=
  match gl with
      CFFGlyph x->CFF.glyph_y1 x
    | OpentypeGlyph x->Opentype.glyph_y1 x

let glyph_x0 gl=
  match gl with
      CFFGlyph x->CFF.glyph_x0 x
    | OpentypeGlyph x->Opentype.glyph_x0 x

let glyph_x1 gl=
  match gl with
      CFFGlyph x->CFF.glyph_x1 x
    | OpentypeGlyph x->Opentype.glyph_x1 x

let select_features a b=match a with
    CFF x->CFF.select_features x b
  | Opentype x->Opentype.select_features x b

let font_features a=match a with
    CFF x->CFF.font_features x
  | Opentype x->Opentype.font_features x


let positioning f glyphs=
  match f with
      CFF x->CFF.positioning x glyphs
    | Opentype x->Opentype.positioning x glyphs

type fontInfo=
    CFFInfo of CFF.fontInfo
  | OpentypeInfo of Opentype.fontInfo

let fontInfo f=
  match f with
      CFF x->CFFInfo (CFF.fontInfo x)
    | Opentype x->OpentypeInfo (Opentype.fontInfo x)

let setName info name=match info with
    CFFInfo f->CFF.setName f name
  | OpentypeInfo f->Opentype.setName f name

let subset font info b c=match font,info with
    CFF x,CFFInfo y->
      CFF.subset x y b c
  | Opentype x,OpentypeInfo y->
    Opentype.subset x y b c
  | _->assert false
