type parameters = {
  format : float * float;
  lead : float;
  measure : float;
  lines_by_page : int;
  left_margin : float;
}
type line= { paragraph:int; lastFigure:int; lineEnd:int; lineStart:int; hyphenStart:int; hyphenEnd:int;
             isFigure:bool; height:int; paragraph_height:int; page:int}

module Line : sig type t = line val compare : 'a -> 'a -> int end
module LineMap :
  sig
    type key = Line.t
    type 'a t = 'a Map.Make(Line).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val max_binding : 'a t -> key * 'a
    val choose : 'a t -> key * 'a
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  end
type 'a kerningBox = 'a FontsTypes.kerningBox
type glyph = {
  contents : CamomileLibrary.UTF8.t;
  glyph : Fonts.glyph;
  width : float;
  x0 : float;
  x1 : float;
  y0 : float;
  y1 : float;
}
type drawing =
    Curve of (float * float * Bezier.curve)
  | Drawing_Box of (float * float * box)
and drawingBox = {
  drawing_x0 : float;
  drawing_x1 : float;
  drawing_y0 : float;
  drawing_y1 : float;
  drawing_contents : drawing list;
}
and glueBox = {
  glue_min_width : float;
  glue_max_width : float;
  glue_badness : float -> float;
}
and hyphenBox = {
  hyphen_normal : box array;
  hyphenated : (box array * box array) array;
}
and box =
    GlyphBox of (float * glyph)
  | Kerning of box kerningBox
  | Glue of glueBox
  | Drawing of drawingBox
  | Hyphen of hyphenBox
  | Mark of int
  | Empty
type error_log=
    Overfull_line of line
  | Widow of line
  | Orphan of line
val print_line : line -> unit
val print_box : box -> unit
val print_text_line : box array array -> line -> unit
val is_glyph : box -> bool
val is_glue : box -> bool
val box_width : float -> box -> float
val box_interval : box -> float * float
val boxes_interval : box array -> float * float
val lower_y : box -> 'a -> float
val upper_y : box -> 'a -> float
val line_height : box array array->line->float*float
val glyphCache_ : glyph Binary.IntMap.t ref Binary.StrMap.t ref
val glyphCache :
  Fonts.font -> Binary.IntMap.key -> CamomileLibrary.UTF8.t -> glyph
val glyph_of_string :
  (FontsTypes.glyph_ids list -> 'a) ->
  ('a -> FontsTypes.glyph_ids list) ->
  Fonts.font -> float -> CamomileLibrary.UTF8.t -> box list
val hyphenate :
  Hyphenate.ptree ->
  (FontsTypes.glyph_ids list -> 'a) ->
  ('a -> FontsTypes.glyph_ids list) ->
  Fonts.font -> float -> CamomileLibrary.UTF8.t -> box list
val knuth_h_badness : float -> float -> float
