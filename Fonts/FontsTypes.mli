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
val glyph_id_cont : glyph_ids -> int
val glyph_id_utf8 : glyph_ids -> CamomileLibrary.UTF8.t
