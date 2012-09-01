exception Glyph_not_found of (string*string)
type 'a kerningBox = {
  advance_height : float;
  advance_width : float;
  kern_x0 : float;
  kern_y0 : float;
  kern_contents : 'a;
}
val empty_kern : 'a->'a kerningBox
type glyph_id = { glyph_utf8 : CamomileLibrary.UTF8.t; glyph_index : int; }
val empty_glyph : glyph_id
type glyph_ids = KernID of glyph_ids kerningBox | GlyphID of glyph_id
val kern : glyph_ids -> glyph_ids kerningBox
val glyph_id_cont : glyph_ids -> int
val glyph_id_utf8 : glyph_ids -> CamomileLibrary.UTF8.t

type subst = { original_glyphs : int array; subst_glyphs : int array; }
type chain = {
  before : int list array;
  input : int list array;
  after : int list array;
}
type substitution =
    Alternative of int array
  | Subst of subst
  | Chain of chain
  | Context of (int * substitution list) array
#ifdef DEBUG
val print_int_array : int array -> unit
val print_int_list : int list -> unit
val print_subst : substitution -> unit
#endif
val apply_ligature : subst -> glyph_id list -> glyph_id list
val apply_subst : subst -> glyph_id list -> glyph_id list
val apply_alternative : int array -> int -> glyph_id list -> glyph_id list
val apply : substitution -> glyph_id list -> glyph_id list
module type Font =
  sig
    type font
    type glyph
    val loadFont : ?offset:int -> ?size:int -> string -> font
    val cardinal : font -> int
    val glyph_of_char : font -> char -> int
    val glyph_of_uchar : font -> CamomileLibrary.UChar.t -> int
    val loadGlyph : font -> ?index:int -> glyph_id -> glyph
    val outlines : glyph -> (float array * float array) list list
    val glyphFont : glyph -> font
    val glyphNumber : glyph -> glyph_id
    val glyphWidth : glyph -> float
    val glyphContents : glyph -> CamomileLibrary.UTF8.t
    val glyph_y0 : glyph -> float
    val glyph_y1 : glyph -> float
    val glyph_x0 : glyph -> float
    val glyph_x1 : glyph -> float
    val fontName : ?index:int -> font -> string
    val glyphName : glyph -> string
    val font_features : font -> string list
    val select_features : font -> string list -> substitution list
    val positioning : font -> glyph_ids list -> glyph_ids list
  end
