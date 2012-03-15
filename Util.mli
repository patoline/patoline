type line = {
  paragraph : int;
  lastFigure : int;
  lineEnd : int;
  lineStart : int;
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
type parameters = {
  measure : float;
  page_height : float;
  left_margin : float;
  local_optimization : int;
  next_acceptable_height : line -> float -> float;
  min_height_before : float;
  min_page_diff : int;
}
val default_params : parameters
module Line : sig type t = line val compare : 'a -> 'a -> int end
module LineMap :
  sig
    type key = Line.t
    type 'a t = 'a New_map.Make(Line).t
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
type 'a kerningBox = 'a Fonts.FTypes.kerningBox
type drawingBox = {
  drawing_min_width : float;
  drawing_nominal_width : float;
  drawing_max_width : float;
  drawing_y0 : float;
  drawing_y1 : float;
  drawing_badness : float -> float;
  drawing_contents : float -> Drivers.contents list;
}
and 'a hyphenBox = {
  hyphen_normal : 'a box array;
  hyphenated : ('a box array * 'a box array) array;
}
and 'a box =
    GlyphBox of Drivers.glyph
  | Kerning of 'a box kerningBox
  | Glue of drawingBox
  | Drawing of drawingBox
  | Hyphen of 'a hyphenBox
  | User of 'a
  | Empty
val drawing : ?offset:float -> Drivers.contents list -> drawingBox
type error_log = Overfull_line of line | Widow of line | Orphan of line
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
val draw_boxes : 'a box list -> Drivers.contents list
val lower_y : 'a box -> 'b -> float
val upper_y : 'a box -> 'b -> float
val line_height : 'a box array array -> line -> float * float
val comp :
  'a box array array -> float -> int -> int -> int -> int -> int -> float
val compression : 'a box array array -> parameters * line -> float
val glyphCache_ : Drivers.glyph Binary.IntMap.t ref Binary.StrMap.t ref
val glyphCache : Fonts.font -> Fonts.FTypes.glyph_id -> Drivers.glyph
val glyph_of_string :
  (Fonts.FTypes.glyph_id list -> Fonts.FTypes.glyph_id list) ->
  (Fonts.FTypes.glyph_ids list -> Fonts.FTypes.glyph_ids list) ->
  Fonts.font -> float -> CamomileLibrary.UTF8.t -> 'a box list
val hyphenate :
  (CamomileLibrary.UTF8.t ->
   (CamomileLibrary.UTF8.t * CamomileLibrary.UTF8.t) array) ->
  (Fonts.FTypes.glyph_id list -> Fonts.FTypes.glyph_id list) ->
  (Fonts.FTypes.glyph_ids list -> Fonts.FTypes.glyph_ids list) ->
  Fonts.font -> float -> CamomileLibrary.UTF8.t -> 'a box list
val knuth_h_badness : float -> float -> float
val glue : float -> float -> float -> 'a box
val resize : float -> 'a box -> 'a box
