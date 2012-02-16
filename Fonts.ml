(** Font types. Only supported right now : OpenType and CFF.

    Here is how to load glyphs from a char :
    [let font=loadFont file in loadGlyph font (glyph_of_char font char)]
 *)

open Binary
open Bezier
open FTypes
open CamomileLibrary


exception Not_supported

module FTypes=FTypes

module type Font=FTypes.Font
module Opentype=Opentype
module CFF=CFF

(** This module is not to be used. It is here to enforce correct
    typing of interface Opentype, while leaving all its interface
    exported by module Opentype above *)
module Opentype_=(Opentype:Font)
(** This module is not to be used. It is here to enforce correct
    typing of interface CFF, while leaving all its interface
    exported by module CFF above *)
module CFF_=(CFF:Font)

(** loadFont pretends it can recognize font file types, but it
    actually only looks at the extension in the file name *)
type font = CFF of CFF.font | Opentype of Opentype.font
type glyph = CFFGlyph of CFF.glyph | OpentypeGlyph of Opentype.glyph
let loadFont ?offset:(off=0) ?size:(_=0) f=
  let size=let i=open_in f in let l=in_channel_length i in close_in i; l in
    if Filename.check_suffix f ".otf" then
      Opentype (Opentype.loadFont ~offset:off f ~size:size)
    else
      raise Not_supported


let glyph_of_uchar f c=
  match f with
      CFF x->CFF.glyph_of_uchar x c
    | Opentype x->Opentype.glyph_of_uchar x c
let glyph_of_char f c=glyph_of_uchar f (UChar.of_char c)


let loadGlyph f g=
  match f with
      CFF x->CFFGlyph (CFF.loadGlyph x g)
    | Opentype x->OpentypeGlyph (Opentype.loadGlyph x g)

let outlines gl=
  match gl with
      CFFGlyph x->CFF.outlines x
    | OpentypeGlyph x->Opentype.outlines x

let glyphFont gl=
  match gl with
      CFFGlyph x->CFF (CFF.glyphFont x)
    | OpentypeGlyph x->Opentype (Opentype.glyphFont x)

let glyphNumber gl=
  match gl with
      CFFGlyph x->CFF.glyphNumber x
    | OpentypeGlyph x->Opentype.glyphNumber x

let glyphWidth gl=
  match gl with
      CFFGlyph x->CFF.glyphWidth x
    | OpentypeGlyph x->Opentype.glyphWidth x

let fontName f=
  match f with
      CFF x->CFF.fontName x
    | Opentype x->Opentype.fontName x

let select_features a b=match a with
    CFF x->CFF.select_features x b
  | Opentype x->Opentype.select_features x b

let positioning f glyphs=
  match f with
      CFF x->CFF.positioning x glyphs
    | Opentype x->Opentype.positioning x glyphs
