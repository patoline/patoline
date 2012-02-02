type 'a kerningBox = {
  advance_height : float;
  advance_width : float;
  kern_x0 : float;
  kern_y0 : float;
  kern_contents : 'a;
}
type glyph_ids =
    KernID of glyph_ids kerningBox
  | GlyphID of (CamomileLibrary.UTF8.t * int)
module type Font =
  sig
    type font
    type glyph
    val loadFont : ?offset:int -> ?size:int -> string -> font
    val glyph_of_char : font -> CamomileLibrary.UChar.t -> int
    val loadGlyph : font -> ?index:int -> int -> glyph
    val outlines : glyph -> (float array * float array) list
    val glyphFont : glyph -> font
    val glyphNumber : glyph -> int
    val glyphWidth : glyph -> float
    val fontName : ?index:int -> font -> string
    val substitutions : font -> glyph_ids list -> glyph_ids list
    val positioning : font -> glyph_ids list -> glyph_ids list
  end
val glyph_id_cont : glyph_ids -> int
val glyph_id_utf8 : glyph_ids -> CamomileLibrary.UTF8.t
