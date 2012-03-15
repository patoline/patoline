exception Not_supported
module FTypes :
  sig
    type 'a kerningBox =
      'a FTypes.kerningBox = {
      advance_height : float;
      advance_width : float;
      kern_x0 : float;
      kern_y0 : float;
      kern_contents : 'a;
    }
    val empty_kern : 'a -> 'a kerningBox
    type glyph_id =
      FTypes.glyph_id = {
      glyph_utf8 : CamomileLibrary.UTF8.t;
      glyph_index : int;
    }
    val empty_glyph : glyph_id
    type glyph_ids =
      FTypes.glyph_ids =
        KernID of glyph_ids kerningBox
      | GlyphID of glyph_id
    val kern : glyph_ids -> glyph_ids kerningBox
    val glyph_id_cont : glyph_ids -> int
    val glyph_id_utf8 : glyph_ids -> CamomileLibrary.UTF8.t
    type subst =
      FTypes.subst = {
      original_glyphs : int array;
      subst_glyphs : int array;
    }
    type chain =
      FTypes.chain = {
      before : Binary.IntSet.t array;
      input : Binary.IntSet.t array;
      after : Binary.IntSet.t array;
    }
    type substitution =
      FTypes.substitution =
        Alternative of int array
      | Subst of subst
      | Chain of chain
      | Context of (int * substitution list) array
    val print_int_array : int array -> unit
    val print_int_list : int list -> unit
    val print_subst : substitution -> unit
    val apply_ligature : glyph_id list -> subst -> glyph_id list
    val apply_subst : glyph_id list -> subst -> glyph_id list
    val apply_alternative :
      glyph_id list -> int array -> int -> glyph_id list
    val apply : glyph_id list -> substitution -> glyph_id list
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
        val fontName : ?index:int -> font -> string
        val font_features : font -> string list
        val select_features : font -> string list -> substitution list
        val positioning : font -> glyph_ids list -> glyph_ids list
      end
    val glyph_roots :
      (float array * float array) list list ->
      int * int * int * int * float list array
  end
module type Font = FTypes.Font
module Opentype :
  sig
    val offsetTable : int
    val dirSize : int
    exception Table_not_found of string
    val tableLookup : string -> in_channel -> int -> int * int
    val tableList : in_channel -> int -> string list
    type font = Opentype.font = CFF of (CFF.font * int)
    val loadFont : ?offset:int -> ?size:int -> string -> font
    val cardinal : font -> int
    type glyph = Opentype.glyph = CFFGlyph of (font * CFF.glyph)
    val glyph_of_uchar : font -> CamomileLibrary.UChar.t -> int
    val glyph_of_char : font -> char -> int
    val glyphFont : glyph -> font
    val loadGlyph : font -> ?index:int -> FTypes.glyph_id -> glyph
    val outlines : glyph -> (float array * float array) list list
    val glyph_y0 : glyph -> float
    val glyph_y1 : glyph -> float
    val glyphNumber : glyph -> FTypes.glyph_id
    val glyphContents : glyph -> CamomileLibrary.UTF8.t
    val glyphWidth : glyph -> float
    val fontName : ?index:int -> font -> string
    val otype_file : font -> in_channel * int
    val coverageIndex : in_channel -> int -> int -> int
    val class_def : in_channel -> int -> int -> int
    val readCoverageIndex : in_channel -> int -> (int * int) list
    val readClass : in_channel -> int -> (int * int) list
    val readLookup : in_channel -> int -> int -> FTypes.substitution list
    val read_gsub : font -> FTypes.substitution list array
    val read_lookup : font -> int -> FTypes.substitution list
    val alternates : string
    val smallCapitals : string
    val caseSensitiveForms : string
    val discretionaryLigatures : string
    val denominators : string
    val fractions : string
    val standardLigatures : string
    val liningFigures : string
    val localizedForms : string
    val numerators : string
    val oldStyleFigures : string
    val ordinals : string
    val ornaments : string
    val proportionalFigures : string
    val stylisticAlternates : string
    val scientificInferiors : string
    val subscript : string
    val superscript : string
    val titling : string
    val tabularFigures : string
    val slashedZero : string
    val select_features : font -> string list -> FTypes.substitution list
    val font_features : font -> string list
    val read_scripts : font -> unit
    val gpos : font -> FTypes.glyph_ids list -> FTypes.glyph_ids list
    val positioning : font -> FTypes.glyph_ids list -> FTypes.glyph_ids list
  end
module CFF :
  sig
    type font =
      CFF.font = {
      file : in_channel;
      offset : int;
      size : int;
      offSize : int;
      nameIndex : int array;
      dictIndex : int array;
      stringIndex : int array;
      subrIndex : string array array;
      gsubrIndex : string array;
    }
    type glyph =
      CFF.glyph = {
      glyphFont : font;
      glyphNumber : FTypes.glyph_id;
      type2 : string;
      matrix : float array;
      subrs : string array;
      gsubrs : string array;
      glyphContents : CamomileLibrary.UTF8.t;
      mutable glyphWidth : float;
      mutable glyphX0 : float;
      mutable glyphX1 : float;
      mutable glyphY0 : float;
      mutable glyphY1 : float;
    }
    val glyphFont : glyph -> font
    val showStack : float array -> int -> unit
    exception Index
    exception Type2Int of int
    val readCFFInt : in_channel -> float
    val index : in_channel -> int -> int array
    val strIndex : in_channel -> int -> string array
    val indexGet : in_channel -> int -> int -> string
    val dict : in_channel -> int -> int -> (int * float list) list
    val findDict : in_channel -> int -> int -> int -> float list
    val loadFont : ?offset:int -> ?size:int -> string -> font
    val glyph_of_uchar : 'a -> 'b -> int
    val glyph_of_char : 'a -> char -> int
    val loadGlyph : font -> ?index:int -> FTypes.glyph_id -> glyph
    val cardinal : font -> int
    exception Found of float
    val outlines_ : glyph -> bool -> (float array * float array) list list
    val outlines : glyph -> (float array * float array) list list
    val glyphWidth : glyph -> float
    val glyphContents : glyph -> CamomileLibrary.UTF8.t
    val compute_bb : glyph -> unit
    val glyph_y0 : glyph -> float
    val glyph_y1 : glyph -> float
    val glyphNumber : glyph -> FTypes.glyph_id
    val fontName : ?index:int -> font -> string
    val fontBBox : ?index:int -> font -> int * int * int * int
    val italicAngle : ?index:int -> font -> float
    val font_features : 'a -> 'b list
    val select_features : 'a -> 'b -> 'c list
    val positioning : 'a -> 'b -> 'b
  end
module Opentype_ : Font
module CFF_ : Font
type font = CFF of CFF.font | Opentype of Opentype.font
type glyph = CFFGlyph of CFF.glyph | OpentypeGlyph of Opentype.glyph
val loadFont : ?offset:int -> ?size:int -> string -> font
val glyph_of_uchar : font -> CamomileLibrary.UChar.t -> int
val glyph_of_char : font -> char -> int
val loadGlyph : font -> FTypes.glyph_id -> glyph
val cardinal : font -> int
val outlines : glyph -> (float array * float array) list list
val glyphFont : glyph -> font
val glyphContents : glyph -> CamomileLibrary.UTF8.t
val glyphNumber : glyph -> FTypes.glyph_id
val glyphWidth : glyph -> float
val glyph_y0 : glyph -> float
val glyph_y1 : glyph -> float
val fontName : font -> string
val select_features : font -> string list -> FTypes.substitution list
val fontFeatures : font -> string list
val positioning : font -> FTypes.glyph_ids list -> FTypes.glyph_ids list
