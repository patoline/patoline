type line = {
  paragraph : int;
  lastFigure : int;
  lineStart : int;
  lineEnd : int;
  hyphenStart : int;
  hyphenEnd : int;
  isFigure : bool;
  mutable height : float;
  paragraph_height : int;
  mutable page_line : int;
  mutable page : int;
  min_width : float;
  nom_width : float;
  max_width : float;
}
val uselessLine : line
type parameters = {
  measure : float;
  page_height : float;
  left_margin : float;
  local_optimization : int;
  next_acceptable_height : line -> parameters->line->parameters-> float;
  min_height_before : float;
  min_height_after : float;
  min_page_before : int;
  min_page_after : int;
}
val default_params : parameters
type 'a kerningBox = 'a Fonts.FTypes.kerningBox
type drawingBox = {
  drawing_min_width : float;
  drawing_nominal_width : float;
  drawing_max_width : float;
  drawing_y0 : float;
  drawing_y1 : float;
  drawing_badness : float -> float;
  drawing_contents : float -> OutputCommon.contents list;
}
and 'a hyphenBox = {
  hyphen_normal : 'a box array;
  hyphenated : ('a box array * 'a box array) array;
}
and 'a box =
    GlyphBox of OutputCommon.glyph
  | Kerning of 'a box kerningBox
  | Glue of drawingBox
  | Drawing of drawingBox
  | Hyphen of 'a hyphenBox
  | User of 'a
  | Empty
val drawing : ?offset:float -> OutputCommon.contents list -> drawingBox
val drawing_blit : drawingBox -> float -> float -> drawingBox -> drawingBox
val print_linef : out_channel -> line -> unit
val print_line : line -> unit
val print_box : 'a box -> unit
val print_box_type : 'a box -> unit
val print_text_line : 'a box array array -> line -> unit
val fold_left_line :
  'a box array array -> ('b -> 'a box -> 'b) -> 'b -> line -> 'b
val first_line : 'a box array array -> line -> 'a box
val last_line : 'a box array array -> line -> 'a box
val is_glyph : 'a box -> bool
val is_glue : 'a box -> bool
val is_hyphen : 'a box -> bool
val box_width : float -> 'a box -> float
val box_interval : 'a box -> float * float * float
val boxes_interval : 'a box array -> float * float * float
val draw_boxes : 'a box list -> OutputCommon.contents list
val lower_y : 'a box -> 'b -> float
val upper_y : 'a box -> 'b -> float
val line_height : 'a box array array -> line -> float * float
val comp :
  'a box array array -> float -> int -> int -> int -> int -> int -> float
val compression : 'a box array array -> parameters * line -> float
val glyphCache_ : OutputCommon.glyph Binary.IntMap.t ref Binary.StrMap.t ref
val glyphCache : Fonts.font -> Fonts.FTypes.glyph_id -> OutputCommon.glyph
val glyph_of_string :
  (Fonts.FTypes.glyph_id list -> Fonts.FTypes.glyph_id list) ->
  (Fonts.FTypes.glyph_ids list -> Fonts.FTypes.glyph_ids list) ->
  Fonts.font ->
  float -> OutputCommon.color -> CamomileLibrary.UTF8.t -> 'a box list
val hyphenate :
  (CamomileLibrary.UTF8.t ->
   (CamomileLibrary.UTF8.t * CamomileLibrary.UTF8.t) array) ->
  (Fonts.FTypes.glyph_id list -> Fonts.FTypes.glyph_id list) ->
  (Fonts.FTypes.glyph_ids list -> Fonts.FTypes.glyph_ids list) ->
  Fonts.font ->
  float -> OutputCommon.color -> CamomileLibrary.UTF8.t -> 'a box list
val knuth_h_badness : float -> float -> float
val glue : float -> float -> float -> 'a box
val resize : float -> 'a box -> 'a box
