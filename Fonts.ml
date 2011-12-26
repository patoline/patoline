open Binary
open Bezier

(*
module type Font = sig
  type font
  type glyph
  val loadFont : string->int->int->font list
  val loadGlyph : font->int->int->glyph
  val outlines : glyph->Bezier.curve list
  val glyphFont:glyph->font
end
*)


type font = CFF of CFF.font | Opentype of Opentype.font
type glyph = CFFGlyph of CFF.glyph | OpentypeGlyph of Opentype.glyph

exception Not_supported

let loadFont ?offset:(off=0) f=
  if Filename.check_suffix f ".otf" then
    Opentype (Opentype.loadFont ~offset:off f)
  else
    if Filename.check_suffix f ".cff" then
      CFF (CFF.loadFont ~offset:off f)
    else
      raise Not_supported

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

let fontName f=
  match f with
      CFF x->CFF.fontName x
    | Opentype x->Opentype.fontName x
