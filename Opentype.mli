exception Table_not_found
type font = CFF of (CFF.font*string*int*int)
type glyph = CFFGlyph of (font*CFF.glyph)
val loadFont : ?offset:int -> string->font
val loadGlyph : font -> ?index:int->int -> glyph
val outlines : glyph -> Bezier.curve list
val glyphFont : glyph -> font

val fontName:?index:int->font -> string
