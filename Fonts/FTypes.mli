type 'a kerningBox = {
  advance_height : float;
  advance_width : float;
  kern_x0 : float;
  kern_y0 : float;
  kern_contents : 'a;
}
type glyph_id = { glyph_utf8 : CamomileLibrary.UTF8.t; glyph_index : int; }
val empty_glyph : glyph_id
type glyph_ids = KernID of glyph_ids kerningBox | GlyphID of glyph_id
val kern : glyph_ids -> glyph_ids kerningBox
val glyph_id_cont : glyph_ids -> int
val glyph_id_utf8 : glyph_ids -> CamomileLibrary.UTF8.t
exception Unknown_feature of string
type features =
    Alternates
  | SmallCapitals
  | CaseSensitiveForms
  | DiscretionaryLigatures
  | Denominators
  | Fractions
  | StandardLigatures
  | LiningFigures
  | LocalizedForms
  | Numerators
  | OldStyleFigures
  | Ordinals
  | Ornaments
  | ProportionalFigures
  | StylisticAlternates
  | ScientificInferiors
  | Subscript
  | Superscript
  | Titling
  | TabularFigures
  | SlashedZero
val str_of_feature : features->string
val print_feature : features -> unit

type subst = { original_glyphs : int array; subst_glyphs : int array; }
type chain = {
  before : Binary.IntSet.t array;
  input : Binary.IntSet.t array;
  after : Binary.IntSet.t array;
}
type substitution =
    Alternative of int array
  | Subst of subst

  | Chain of chain
  | Context of (int * substitution list) array
val print_int_array : int array -> unit
val print_int_list : int list -> unit
val print_subst : substitution -> unit

val apply_subst : glyph_id list -> subst -> glyph_id list
val apply_alternative : glyph_id list -> int array -> int -> glyph_id list
val apply : glyph_id list -> substitution -> glyph_id list
module type Font =
  sig
    type font
    type glyph
    val loadFont : ?offset:int -> ?size:int -> string -> font
    val cardinal : font->int
    val glyph_of_char : font -> char -> int
    val glyph_of_uchar : font -> CamomileLibrary.UChar.t -> int
    val loadGlyph : font -> ?index:int -> glyph_id -> glyph
    val outlines : glyph -> (float array * float array) list
    val glyphFont : glyph -> font
    val glyphNumber : glyph -> glyph_id
    val glyphWidth : glyph -> float
    val fontName : ?index:int -> font -> string
    val font_features : font -> features list
    val select_features : font -> features list -> substitution list
    val positioning : font -> glyph_ids list -> glyph_ids list
  end
