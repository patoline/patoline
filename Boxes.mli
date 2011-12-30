open Batteries

type glyphBox = { contents : UTF8.t; glyph : Fonts.glyph; width : float; }
type box = GlyphBox of glyphBox | Glue of (float * float * float)
val current_font : Fonts.font ref
val current_size : int ref
val isGlue : box -> bool

val lineBreak :
  ?format:float * float ->
  ?lead:float ->
  ?measure:float ->
  ?figures:'a array -> box DynArray.t DynArray.t -> box list list
