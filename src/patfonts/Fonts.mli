(** To understand this interface, one must understand the
    difference between a glyph and a character. While most UTF8
    characters can have corresponding glyphs, a glyph may not
    represent exactly one character. Sometimes it will represent
    more than one character (as in the case of ligatures) and
    sometimes none (as in the case of ornaments). To ease the
    manipulation of glyphs, this modules allows to manipulate
    integers representing their indices in the font, for instance
    to write glyph substitutions more easily, and also to load
    informations such as their outlines. *)

(** Type font *)
open Extra
open FTypes

type font

val uniqueName : font -> string

val fontName : ?index:int -> font -> name

(** Loads a font in the memory from a file name. Actual
    implementations may keep the file open *)
val loadFont : ?offset:int -> ?size:int option -> string -> font

val cardinal : font -> int

(** Computes the index of a glyph corresponding to a given
    character in the font, but loses the link between the character
    and the glyph. It is your responsibility to maintain this link
    with the [glyph_id] type *)
val glyph_of_uchar : font -> UChar.t -> int
val glyph_of_char  : font -> char    -> int


type glyph

(** Load the actual glyph, which gives more precise knownledge
    about the glyph : its width and outlines, for instance *)
val loadGlyph : font -> ?index:int -> glyph_id -> glyph

(** Outlines of the glyph as a list of Bezier curves, each given
    by two arrays of coefficients of Bernstein polynomials. The
    degree of the polynomial is the length of the array minus
    one. *)
val outlines : glyph -> (float array * float array) list list

val glyphFont : glyph -> font

val glyphNumber : glyph -> glyph_id

val glyphWidth : glyph -> float

val glyphContents : glyph -> string

val glyph_y0 : glyph -> float
val glyph_y1 : glyph -> float
val glyph_x0 : glyph -> float
val glyph_x1 : glyph -> float

val glyphName : glyph -> string

(** Lists all the available features of the font *)
val font_features : font -> string list


type feature_set

(** Converts a given list of features into a list of corresponding substitutions *)
val select_features : font -> string list -> feature_set

val apply_features : font -> feature_set -> glyph_id list -> glyph_id list

(** Appiles the available positioning information to a glyph
    list. This can be used for kerning, but not only *)
val positioning : font -> glyph_ids list -> glyph_ids list


type fontInfo

val fontInfo : font -> fontInfo

val subset : font -> fontInfo -> int IntMap.t -> glyph_id array -> Buffer.t

val setName : fontInfo -> name -> unit

val add_kerning : fontInfo -> (IntMap.key * int * 'a FTypes.kerningBox *
                               'b FTypes.kerningBox) list -> unit

val cff_only : font -> CFF.font

val is_cff : font -> bool
