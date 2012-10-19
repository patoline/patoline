type box =
    GlyphBox of OutputCommon.glyph
  | Kerning of box Fonts.FTypes.kerningBox
  | Glue of drawingBox
  | Drawing of drawingBox
  | Hyphen of hyphenBox
  | User of user
  | BeginFigure of int
  | FlushFigure of int
  | Parameters of (Line.parameters -> Line.parameters)
  | Empty
and drawingBox = {
  drawing_min_width : float;
  drawing_nominal_width : float;
  drawing_max_width : float;
  drawing_y0 : float;
  drawing_y1 : float;
  drawing_badness : float -> float;
  drawing_contents : float -> OutputCommon.contents list;
}
and hyphenBox = {
  hyphen_normal : box array;
  hyphenated : (box array * box array) array;
}
and user =
    Label of string
  | FigureRef of int
  | Pageref of string
  | Structure of int list
  | Footnote of int * drawingBox
  | BeginURILink of string
  | BeginLink of string
  | EndLink
  | AlignmentMark
module UserMap :
  sig
    type key = user
    type +'a t
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
val drawing : ?offset:float -> OutputCommon.contents list -> drawingBox
val drawing_inline :
  ?offset:float -> OutputCommon.contents list -> drawingBox
val drawing_blit : drawingBox -> float -> float -> drawingBox -> drawingBox
val is_glyph : box -> bool
val is_glue : box -> bool
val is_hyphen : box -> bool
val box_width : float -> box -> float
val box_size : box -> float
val box_interval : box -> float * float * float
val boxes_interval : box array -> float * float * float
val lower_y : box -> float
val upper_y : box -> float
val knuth_h_badness : float -> float -> float
val glue : float -> float -> float -> box
val resize : float -> box -> box
val fold_left_line :
  box array array -> ('a -> box -> 'a) -> 'a -> Line.line -> 'a
val fold_left : ('a -> box -> 'a) -> 'a -> box list -> 'a
val first_line : box array array -> Line.line -> box
val last_line : box array array -> Line.line -> box
val line_height :
  box array array -> drawingBox array -> Line.line -> float * float
val comp :
  box array array -> float -> int -> int -> int -> int -> int -> float
val compression : box array array -> Line.parameters -> Line.line -> float
val glyphCache_ : OutputCommon.glyph Util.IntMap.t ref Util.StrMap.t ref
val glyphCache : Fonts.font -> Fonts.FTypes.glyph_id -> OutputCommon.glyph
val glyph_of_string :
  (Fonts.FTypes.glyph_id list -> Fonts.FTypes.glyph_id list) ->
  (Fonts.FTypes.glyph_ids list -> Fonts.FTypes.glyph_ids list) ->
  Fonts.font ->
  float -> OutputCommon.color -> CamomileLibrary.UTF8.t -> box list
val hyphenate :
  (CamomileLibrary.UTF8.t ->
   (CamomileLibrary.UTF8.t * CamomileLibrary.UTF8.t) array) ->
  (Fonts.FTypes.glyph_id list -> Fonts.FTypes.glyph_id list) ->
  (Fonts.FTypes.glyph_ids list -> Fonts.FTypes.glyph_ids list) ->
  Fonts.font ->
  float -> OutputCommon.color -> CamomileLibrary.UTF8.t -> box list
val print_box : out_channel -> box -> unit
val print_box_type : out_channel -> box -> unit
val print_text_line : box array array -> Line.line -> unit
val text_box : box -> string
val text_line : box array array -> Line.line -> string
